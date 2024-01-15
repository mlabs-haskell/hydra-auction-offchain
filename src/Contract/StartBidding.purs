module HydraAuctionOffchain.Contract.StartBidding
  ( StartBiddingContractError
      ( StartBidding_Error_InvalidAuctionTerms
      , StartBidding_Error_CouldNotGetOwnAddress
      , StartBidding_Error_ContractNotInitiatedBySeller
      , StartBidding_Error_CurrentTimeBeforeBiddingStart
      , StartBidding_Error_CurrentTimeAfterBiddingEnd
      , StartBidding_Error_CouldNotBuildAuctionValidators
      , StartBidding_Error_InvalidAuctionInfo
      , StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo
      )
  , StartBiddingContractParams(StartBiddingContractParams)
  , mkStartBiddingContractWithErrors
  , startBiddingContract
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash, toPubKeyHash)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, OutputDatum(OutputDatum), Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTimeRange, mkFiniteInterval)
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
import Contract.Wallet (getWalletAddress)
import Control.Error.Util ((!?))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Map (singleton, toUnfoldable) as Map
import Data.Maybe (fromJust)
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
  , AuctionInfoValidationError
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
  , validateAuctionInfo
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import Partial.Unsafe (unsafePartial)

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
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< StartBidding_Error_InvalidAuctionTerms) pure

  -- Check that the contract is initiated by the seller:
  ownAddress <- getWalletAddress !? StartBidding_Error_CouldNotGetOwnAddress
  when (ownAddress /= auctionTermsRec.sellerAddress) $
    throwError StartBidding_Error_ContractNotInitiatedBySeller

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError StartBidding_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError StartBidding_Error_CurrentTimeAfterBiddingEnd

  -- Build validators:
  validators <-
    withExceptT StartBidding_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  -- Check auction info:
  validateAuctionInfo auctionInfo validators #
    validation (throwError <<< StartBidding_Error_InvalidAuctionInfo) pure

  -- Query current auction escrow utxo:
  auctionEscrowUtxo <- queryAuctionEscrowUtxo auctionInfo
    !? StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo

  let
    validatorHashes = unwrap $ validatorHash <$> validators

    mkAuctionToken :: TokenName -> Value
    mkAuctionToken tokenName = Value.singleton auctionCs tokenName one

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

    txValidRange :: POSIXTimeRange
    txValidRange =
      mkFiniteInterval nowTime
        (auctionTermsRec.biddingEnd - wrap (BigInt.fromInt 1000))

    sellerPkh :: PaymentPubKeyHash
    sellerPkh = wrap $ unsafePartial fromJust $ toPubKeyHash auctionTermsRec.sellerAddress

    constraints :: TxConstraints Void Void
    constraints = mconcat
      [ -- Spend auction escrow utxo:
        Constraints.mustSpendScriptOutput auctionEscrowOref auctionEscrowRedeemer

      , -- Lock auction lot with auction state token at auction escrow
        -- validator address, set state to BiddingStarted:
        Constraints.mustPayToScript validatorHashes.auctionEscrow auctionEscrowDatum
          DatumInline
          auctionEscrowValue

      , -- Lock standing bid token with empty bid state datum at
        -- standing bid validator address:
        Constraints.mustPayToScript validatorHashes.standingBid standingBidDatum DatumInline
          standingBidTokenValue

      , -- This transaction must be signed by the seller:
        Constraints.mustBeSignedBy sellerPkh

      , -- Set transaction validity interval to bidding period:
        Constraints.mustValidateIn txValidRange
      ]

    lookups :: ScriptLookups Void
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton auctionEscrowOref $ snd auctionEscrowUtxo
      , Lookups.validator (unwrap validators).auctionEscrow
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
  | StartBidding_Error_CouldNotGetOwnAddress
  | StartBidding_Error_ContractNotInitiatedBySeller
  | StartBidding_Error_CurrentTimeBeforeBiddingStart
  | StartBidding_Error_CurrentTimeAfterBiddingEnd
  | StartBidding_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | StartBidding_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)
  | StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo

derive instance Generic StartBiddingContractError _
derive instance Eq StartBiddingContractError

instance Show StartBiddingContractError where
  show = genericShow

instance ToContractError StartBiddingContractError where
  toContractError = wrap <<< case _ of
    StartBidding_Error_InvalidAuctionTerms errors ->
      { errorCode: "StartBidding01"
      , message: "Invalid auction terms, errors: " <> show errors <> "."
      }
    StartBidding_Error_CouldNotGetOwnAddress ->
      { errorCode: "StartBidding02"
      , message: "Could not get own address."
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
    StartBidding_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "StartBidding06"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
    StartBidding_Error_InvalidAuctionInfo errors ->
      { errorCode: "StartBidding07"
      , message: "Invalid auction info, errors: " <> show errors <> "."
      }
    StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo ->
      { errorCode: "StartBidding08"
      , message: "Could not find current auction escrow utxo."
      }
