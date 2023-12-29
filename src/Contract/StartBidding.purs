module HydraAuctionOffchain.Contract.StartBidding
  ( StartBiddingContractError
      ( StartBidding_Error_InvalidAuctionTerms
      , StartBidding_Error_CouldNotGetOwnPubKeyHash
      , StartBidding_Error_ContractNotInitiatedBySeller
      , StartBidding_Error_CurrentTimeBeforeBiddingStart
      , StartBidding_Error_CurrentTimeAfterBiddingEnd
      , StartBidding_Error_CouldNotBuildAuctionEscrowValidator
      , StartBidding_Error_AuctionEscrowValidatorAddressMismatch
      , StartBidding_Error_CouldNotGetStandingBidValidatorHash
      , StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo
      )
  , StartBiddingContractParams(StartBiddingContractParams)
  , mkStartBiddingContractWithErrors
  , startBiddingContract
  ) where

import Contract.Prelude

import Contract.Address (addressPaymentValidatorHash, scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, OutputDatum(OutputDatum), Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutput
  , mustValidateIn
  ) as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value (leq, singleton) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find) as Array
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Map (singleton, toUnfoldable) as Map
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Codec (class HasJson)
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
  , ContractOutput
  , ContractResult
  , StandingBidState(StandingBidState)
  , Utxo
  , auctionInfoCodec
  , emptySubmitTxData
  , mkContractOutput
  , submitTxReturningContractResult
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (biddingPeriod)
import HydraAuctionOffchain.Contract.Validators
  ( MkAuctionEscrowValidatorError
  , mkAuctionEscrowValidatorFromAuctionInfo
  )

newtype StartBiddingContractParams = StartBiddingContractParams
  { auctionInfo :: AuctionInfo
  }

derive instance Generic StartBiddingContractParams _
derive instance Newtype StartBiddingContractParams _
derive instance Eq StartBiddingContractParams

instance Show StartBiddingContractParams where
  show = genericShow

instance HasJson StartBiddingContractParams where
  jsonCodec = const startBiddingContractParamsCodec

startBiddingContractParamsCodec :: CA.JsonCodec StartBiddingContractParams
startBiddingContractParamsCodec =
  wrapIso StartBiddingContractParams $ CA.object "StartBiddingContractParams" $
    CAR.record
      { auctionInfo: auctionInfoCodec
      }

startBiddingContract
  :: StartBiddingContractParams
  -> Contract (ContractOutput TransactionHash)
startBiddingContract =
  mkContractOutput _.txHash <<< mkStartBiddingContractWithErrors

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

  -- Check that the contract is initiated by the seller:
  ownPkh <- ownPaymentPubKeyHash !? StartBidding_Error_CouldNotGetOwnPubKeyHash
  when (unwrap ownPkh /= auctionTermsRec.sellerPkh) $
    throwError StartBidding_Error_ContractNotInitiatedBySeller

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError StartBidding_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError StartBidding_Error_CurrentTimeAfterBiddingEnd

  -- Build auction escrow validator:
  auctionEscrowValidator <-
    withExceptT StartBidding_Error_CouldNotBuildAuctionEscrowValidator $
      mkAuctionEscrowValidatorFromAuctionInfo auctionInfo
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

      , -- This transaction must be signed by the seller:
        Constraints.mustBeSignedBy $ wrap auctionTermsRec.sellerPkh

      , -- Set transaction validity interval to bidding period:
        Constraints.mustValidateIn $ biddingPeriod auctionTerms
      ]

    lookups :: ScriptLookups Void
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton auctionEscrowOref $ snd auctionEscrowUtxo
      , Lookups.validator auctionEscrowValidator
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
  | StartBidding_Error_CouldNotGetOwnPubKeyHash
  | StartBidding_Error_ContractNotInitiatedBySeller
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
    StartBidding_Error_CouldNotGetOwnPubKeyHash ->
      { errorCode: "StartBidding02"
      , message: "Could not get own public key hash."
      }
    StartBidding_Error_ContractNotInitiatedBySeller ->
      { errorCode: "StartBidding03"
      , message: "Contract must be initiated by the seller."
      }
    StartBidding_Error_CurrentTimeBeforeBiddingStart ->
      { errorCode: "StartBidding04"
      , message: "Tx cannot be submitted before bidding start time."
      }
    StartBidding_Error_CurrentTimeAfterBiddingEnd ->
      { errorCode: "StartBidding05"
      , message: "Tx cannot be submitted after bidding end time."
      }
    StartBidding_Error_CouldNotBuildAuctionEscrowValidator err ->
      { errorCode: "StartBidding06"
      , message: "Could not build auction escrow validator, error: " <> show err <> "."
      }
    StartBidding_Error_AuctionEscrowValidatorAddressMismatch ->
      { errorCode: "StartBidding07"
      , message:
          "Computed auction escrow validator address doesn't match \
          \the address provided in AuctionInfo."
      }
    StartBidding_Error_CouldNotGetStandingBidValidatorHash ->
      { errorCode: "StartBidding08"
      , message: "Impossible: Could not get standing bid validator hash."
      }
    StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo ->
      { errorCode: "StartBidding09"
      , message: "Could not find current auction escrow utxo."
      }
