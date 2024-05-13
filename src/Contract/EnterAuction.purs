module HydraAuctionOffchain.Contract.EnterAuction
  ( EnterAuctionContractError
      ( EnterAuction_Error_InvalidAuctionTerms
      , EnterAuction_Error_CurrentTimeAfterBiddingEnd
      , EnterAuction_Error_CouldNotBuildAuctionValidators
      , EnterAuction_Error_InvalidAuctionInfo
      , EnterAuction_Error_CouldNotGetOwnPubKey
      )
  , EnterAuctionContractParams(EnterAuctionContractParams)
  , enterAuctionContract
  , mkEnterAuctionContractWithErrors
  ) where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, toData)
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Time (POSIXTimeRange, mkFiniteInterval)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustMintCurrencyUsingNativeScript
  , mustPayToScript
  , mustValidateIn
  ) as Constraints
import Contract.Value (Value)
import Contract.Value (lovelaceValueOf) as Value
import Control.Error.Util (bool)
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Codec (class HasJson, bigIntCodec)
import HydraAuctionOffchain.Contract.PersonalOracle (PersonalOracle, mkPersonalOracle)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , ActorRole(ActorRoleBidder)
  , AuctionActor(AuctionActor)
  , AuctionInfoExtended(AuctionInfoExtended)
  , AuctionInfoValidationError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , BidderInfo(BidderInfo)
  , ContractOutput
  , ContractResult
  , assetToValue
  , auctionInfoExtendedCodec
  , emptySubmitTxData
  , mkContractOutput
  , submitTxReturningContractResult
  , validateAuctionInfo
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Wallet (SignMessageError, askWalletVk')
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt

newtype EnterAuctionContractParams = EnterAuctionContractParams
  { auctionInfo :: AuctionInfoExtended
  , depositAmount :: Maybe BigInt
  }

derive instance Generic EnterAuctionContractParams _
derive instance Newtype EnterAuctionContractParams _
derive instance Eq EnterAuctionContractParams

instance Show EnterAuctionContractParams where
  show = genericShow

instance HasJson EnterAuctionContractParams where
  jsonCodec = const enterAuctionContractParamsCodec

enterAuctionContractParamsCodec :: CA.JsonCodec EnterAuctionContractParams
enterAuctionContractParamsCodec =
  wrapIso EnterAuctionContractParams $ CA.object "EnterAuctionContractParams" $
    CAR.record
      { auctionInfo: auctionInfoExtendedCodec
      , depositAmount: CA.maybe bigIntCodec
      }

enterAuctionContract
  :: EnterAuctionContractParams
  -> Contract (ContractOutput TransactionHash)
enterAuctionContract =
  mkContractOutput _.txHash <<< mkEnterAuctionContractWithErrors

mkEnterAuctionContractWithErrors
  :: EnterAuctionContractParams
  -> ExceptT EnterAuctionContractError Contract ContractResult
mkEnterAuctionContractWithErrors (EnterAuctionContractParams params) = do
  let
    auctionInfo@(AuctionInfoExtended auctionInfoRec) = params.auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms
    depositAmount = fromMaybe auctionTermsRec.minDepositAmount params.depositAmount

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< EnterAuction_Error_InvalidAuctionTerms) pure

  -- Check that the current time < bidding end time: 
  nowTime <- lift currentTime
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError EnterAuction_Error_CurrentTimeAfterBiddingEnd

  -- Build validators:
  validators <-
    withExceptT EnterAuction_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  -- Check auction info:
  validateAuctionInfo auctionInfoRec validators #
    validation (throwError <<< EnterAuction_Error_InvalidAuctionInfo) pure

  -- Get bidder vkey and address; display warning to the user if the provided
  -- deposit amount does not satisfy minDepositAmount:
  let
    isLowDeposit = depositAmount < auctionTermsRec.minDepositAmount
    lowDepositMessage =
      " WARNING: The provided deposit amount does not meet the minimum deposit \
      \requirement for this auction."
  { vkey: bidderVk, address: bidderAddress, pkh: bidderPkh } <-
    withExceptT EnterAuction_Error_CouldNotGetOwnPubKey $
      askWalletVk' (bool mempty lowDepositMessage isLowDeposit)

  let
    -- BidderDeposit -------------------------------------------------

    bidderDepositValidatorHash :: ValidatorHash
    bidderDepositValidatorHash = validatorHash (unwrap validators).bidderDeposit

    bidderDepositValue :: Value
    bidderDepositValue = Value.lovelaceValueOf depositAmount

    bidderInfoDatum :: Datum
    bidderInfoDatum = wrap $ toData $ BidderInfo
      { bidderAddress
      , bidderVk
      }

    -- AuctionActor --------------------------------------------------

    bidderOracle :: PersonalOracle
    bidderOracle = mkPersonalOracle $ wrap bidderPkh

    bidderOracleTokenValue :: Value
    bidderOracleTokenValue = assetToValue bidderOracle.assetClass one

    auctionActorDatum :: Datum
    auctionActorDatum = wrap $ toData $ AuctionActor
      { auctionInfo
      , role: ActorRoleBidder
      }

    --

    txValidRange :: POSIXTimeRange
    txValidRange =
      mkFiniteInterval nowTime
        (auctionTermsRec.biddingEnd - wrap (BigInt.fromInt 1000))

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Lock deposit with BidderInfo datum under the auction's
        -- bidder deposit validator:
        Constraints.mustPayToScript bidderDepositValidatorHash bidderInfoDatum DatumInline
          bidderDepositValue

      -- Mint bidder's personal oracle token:
      , Constraints.mustMintCurrencyUsingNativeScript bidderOracle.nativeScript
          (unwrap bidderOracle.assetClass).tokenName
          one

      -- Lock auction actor datum with personal oracle token at 
      -- the bidder's personal oracle address:
      , Constraints.mustPayToScript (wrap $ bidderOracle.nativeScriptHash) auctionActorDatum
          DatumInline
          bidderOracleTokenValue

      , -- Transaction must be signed by the bidder:
        Constraints.mustBeSignedBy $ wrap bidderPkh

      , -- Transaction must validate before the bidding end time:
        Constraints.mustValidateIn txValidRange
      ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { constraints = constraints
    }

----------------------------------------------------------------------
-- Errors

data EnterAuctionContractError
  = EnterAuction_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | EnterAuction_Error_CurrentTimeAfterBiddingEnd
  | EnterAuction_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | EnterAuction_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)
  | EnterAuction_Error_CouldNotGetOwnPubKey SignMessageError

derive instance Generic EnterAuctionContractError _
derive instance Eq EnterAuctionContractError

instance Show EnterAuctionContractError where
  show = genericShow

instance ToContractError EnterAuctionContractError where
  toContractError = wrap <<< case _ of
    EnterAuction_Error_InvalidAuctionTerms errors ->
      { errorCode: "EnterAuction01"
      , message: "Invalid auction terms, errors: " <> show errors <> "."
      }
    EnterAuction_Error_CurrentTimeAfterBiddingEnd ->
      { errorCode: "EnterAuction02"
      , message: "Tx cannot be submitted after bidding end time."
      }
    EnterAuction_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "EnterAuction03"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
    EnterAuction_Error_InvalidAuctionInfo errors ->
      { errorCode: "EnterAuction04"
      , message: "Invalid auction info, errors: " <> show errors <> "."
      }
    EnterAuction_Error_CouldNotGetOwnPubKey err ->
      { errorCode: "EnterAuction05"
      , message: "Could not get own public key, error: " <> show err <> "."
      }
