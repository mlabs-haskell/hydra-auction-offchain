module HydraAuctionOffchain.Contract.StartBidding where

import Contract.Prelude

import Contract.Address (addressPaymentValidatorHash, scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, OutputDatum(OutputDatum), Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutput(TransactionOutput))
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints (mustPayToScript, mustSpendScriptOutput) as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value (leq, singleton) as Value
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find) as Array
import Data.Map (singleton, toUnfoldable) as Map
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Contract.MintingPolicies
  ( auctionEscrowTokenName
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionEscrowRedeemer(StartBiddingRedeemer)
  , AuctionEscrowState(AuctionAnnounced, BiddingStarted)
  , AuctionInfo(AuctionInfo)
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , ContractResult
  , StandingBidState(StandingBidState)
  , Utxo
  , emptySubmitTxData
  , submitTxReturningContractResult
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators
  ( MkAuctionEscrowValidatorError
  , mkAuctionEscrowValidator
  )

newtype StartBiddingContractParams = StartBiddingContractParams
  { auctionInfo :: AuctionInfo
  }

mkStartBiddingContractWithErrors
  :: StartBiddingContractParams
  -> ExceptT StartBiddingContractError Contract ContractResult
mkStartBiddingContractWithErrors (StartBiddingContractParams params) = do
  let
    auctionInfo@(AuctionInfo auctionInfoRec) = params.auctionInfo
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< StartBidding_Error_InvalidAuctionTerms) pure

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError StartBidding_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError StartBidding_Error_CurrentTimeAfterBiddingEnd

  -- Build auction escrow validator:
  auctionEscrowValidator <-
    withExceptT StartBidding_Error_CouldNotBuildAuctionEscrowValidator $
      mkAuctionEscrowValidator auctionInfo
  let
    auctionEscrowValidatorHash = validatorHash auctionEscrowValidator
    auctionEscrowValidatorAddress = scriptHashAddress auctionEscrowValidatorHash Nothing

  -- Check that the computed auction escrow validator address matches
  -- the address specified in auction info:
  when (auctionEscrowValidatorAddress /= auctionInfoRec.auctionEscrowAddr) $
    throwError StartBidding_Error_AuctionEscrowValidatorAddressMismatch

  -- Get standing bid validator hash:
  standingBidValidatorHash <- addressPaymentValidatorHash auctionInfoRec.standingBidAddr
    ?? StartBidding_Error_CouldNotGetStandingBidValidatorHash

  -- Query current auction escrow utxo:
  auctionEscrowUtxo <- queryAuctionEscrowUtxo auctionInfo
    !? StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo

  let
    mkAuctionToken :: TokenName -> Value
    mkAuctionToken tokenName = Value.singleton auctionInfoRec.auctionId tokenName one

    -- AuctionEscrow -------------------------------------------------

    auctionEscrowOref :: TransactionInput
    auctionEscrowOref = fst auctionEscrowUtxo

    auctionEscrowRedeemer :: Redeemer
    auctionEscrowRedeemer = wrap $ toData StartBiddingRedeemer

    auctionEscrowDatum :: Datum
    auctionEscrowDatum = wrap $ toData BiddingStarted

    auctionEscrowValue :: Value
    auctionEscrowValue = auctionTermsRec.auctionLot <> mkAuctionToken auctionEscrowTokenName

    -- StandingBid ---------------------------------------------------

    standingBidDatum :: Datum
    standingBidDatum = wrap $ toData $ StandingBidState Nothing

    standingBidTokenValue :: Value
    standingBidTokenValue = mkAuctionToken standingBidTokenName

    --

    constraints :: TxConstraints Void Void
    constraints = mconcat
      [ -- Spend auction escrow utxo:
        Constraints.mustSpendScriptOutput auctionEscrowOref auctionEscrowRedeemer

      , -- Lock auction lot with auction state token at auction escrow
        -- validator address, set state to BiddingStarted:
        Constraints.mustPayToScript auctionEscrowValidatorHash auctionEscrowDatum DatumInline
          auctionEscrowValue

      , -- Lock standing bid token with empty bid state datum at
        -- standing bid validator address:
        Constraints.mustPayToScript standingBidValidatorHash standingBidDatum DatumInline
          standingBidTokenValue
      ]

    lookups :: ScriptLookups Void
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton auctionEscrowOref $ snd auctionEscrowUtxo
      ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

queryAuctionEscrowUtxo :: AuctionInfo -> Contract (Maybe Utxo)
queryAuctionEscrowUtxo (AuctionInfo auctionInfo) =
  utxosAt auctionInfo.auctionEscrowAddr
    <#> Array.find (isCurrentAuctionEscrowUtxo <<< _.output <<< unwrap <<< snd)
    <<< Map.toUnfoldable
  where
  AuctionTerms auctionTerms = auctionInfo.auctionTerms

  isCurrentAuctionEscrowUtxo :: TransactionOutput -> Boolean
  isCurrentAuctionEscrowUtxo (TransactionOutput txOut) =
    auctionEscrowValue `Value.leq` txOut.amount
      && (txOut.datum == OutputDatum (wrap $ toData AuctionAnnounced))
      && isNothing txOut.referenceScript

  auctionEscrowValue :: Value
  auctionEscrowValue =
    auctionTerms.auctionLot <> mkAuctionToken auctionEscrowTokenName
      <> mkAuctionToken standingBidTokenName

  mkAuctionToken :: TokenName -> Value
  mkAuctionToken tokenName =
    Value.singleton auctionInfo.auctionId tokenName one

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data StartBiddingContractError
  = StartBidding_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | StartBidding_Error_CurrentTimeBeforeBiddingStart
  | StartBidding_Error_CurrentTimeAfterBiddingEnd
  | StartBidding_Error_CouldNotBuildAuctionEscrowValidator MkAuctionEscrowValidatorError
  | StartBidding_Error_AuctionEscrowValidatorAddressMismatch --
  | StartBidding_Error_CouldNotGetStandingBidValidatorHash
  | StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo

derive instance Generic StartBiddingContractError _
derive instance Eq StartBiddingContractError

instance Show StartBiddingContractError where
  show = genericShow

instance ToContractError StartBiddingContractError where
  toContractError = wrap <<< case _ of
    StartBidding_Error_InvalidAuctionTerms validationErrors ->
      { errorCode: "StartBidding01"
      , message: "Invalid auction terms, errors: " <> show validationErrors <> "."
      }
    StartBidding_Error_CurrentTimeBeforeBiddingStart ->
      { errorCode: "StartBidding02"
      , message: "Tx cannot be submitted before bidding start time."
      }
    StartBidding_Error_CurrentTimeAfterBiddingEnd ->
      { errorCode: "StartBidding03"
      , message: "Tx cannot be submitted after bidding end time."
      }
    StartBidding_Error_CouldNotBuildAuctionEscrowValidator err ->
      { errorCode: "StartBidding04"
      , message: "Could not build auction escrow validator, error: " <> show err <> "."
      }
    StartBidding_Error_AuctionEscrowValidatorAddressMismatch ->
      { errorCode: "StartBidding05"
      , message:
          "Computed auction escrow validator address doesn't match \
          \the address provided in AuctionInfo."
      }
    StartBidding_Error_CouldNotGetStandingBidValidatorHash ->
      { errorCode: "StartBidding06"
      , message: "Impossible: Could not get standing bid validator hash."
      }
    StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo ->
      { errorCode: "StartBidding07"
      , message: "Could not find current auction escrow utxo."
      }
