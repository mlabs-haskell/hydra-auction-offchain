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
      , StartBidding_Error_AuctionLotValueConversionFailure
      )
  , StartBiddingContractParams(StartBiddingContractParams)
  , mkStartBiddingContractWithErrors
  , startBiddingContract
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address (fromCardano) as Plutus.Address
import Cardano.Plutus.Types.Value (toCardano) as Plutus.Value
import Cardano.Types (NetworkId, PaymentPubKeyHash, PlutusData, RedeemerDatum)
import Cardano.Types.BigNum (one) as BigNum
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTimeRange, mkFiniteInterval)
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutput
  , mustValidateIn
  ) as Constraints
import Contract.Value (TokenName, Value)
import Contract.Value (singleton) as Value
import Contract.Wallet (getWalletAddress)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Map (singleton) as Map
import Data.Maybe (fromJust)
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Contract.MintingPolicies
  ( auctionEscrowTokenName
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.QueryUtxo (queryAuctionEscrowUtxo)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionEscrowRedeemer(StartBiddingRedeemer)
  , AuctionEscrowState(AuctionAnnounced, BiddingStarted)
  , AuctionInfoExtended(AuctionInfoExtended)
  , AuctionInfoValidationError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , ContractOutput
  , ContractResult
  , StandingBidState(StandingBidState)
  , auctionInfoExtendedCodec
  , emptySubmitTxData
  , mkContractOutput
  , submitTxReturningContractResult
  , validateAuctionInfo
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Lib.Plutus.Address (toPubKeyHash)
import JS.BigInt (fromInt) as BigInt
import Partial.Unsafe (unsafePartial)

newtype StartBiddingContractParams = StartBiddingContractParams
  { auctionInfo :: AuctionInfoExtended
  }

derive instance Generic StartBiddingContractParams _
derive instance Newtype StartBiddingContractParams _
derive instance Eq StartBiddingContractParams

instance Show StartBiddingContractParams where
  show = genericShow

instance HasJson StartBiddingContractParams NetworkId where
  jsonCodec network = const (startBiddingContractParamsCodec network)

startBiddingContractParamsCodec :: NetworkId -> CA.JsonCodec StartBiddingContractParams
startBiddingContractParamsCodec network =
  wrapIso StartBiddingContractParams $ CA.object "StartBiddingContractParams" $
    CAR.record
      { auctionInfo: auctionInfoExtendedCodec network
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
    (AuctionInfoExtended auctionInfoRec) = params.auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< StartBidding_Error_InvalidAuctionTerms) pure

  -- Convert auction lot Value:
  auctionLotValue <- Plutus.Value.toCardano auctionTermsRec.auctionLot
    ?? StartBidding_Error_AuctionLotValueConversionFailure

  -- Check that the contract is initiated by the seller:
  ownAddress <- getWalletAddress !? StartBidding_Error_CouldNotGetOwnAddress
  when (Plutus.Address.fromCardano ownAddress /= Just auctionTermsRec.sellerAddress) $
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
  validateAuctionInfo auctionInfoRec validators #
    validation (throwError <<< StartBidding_Error_InvalidAuctionInfo) pure

  -- Query current auction escrow utxo:
  auctionEscrowUtxo <- queryAuctionEscrowUtxo AuctionAnnounced auctionInfoRec
    !? StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo

  let
    validatorHashes = unwrap $ validatorHash <$> validators

    mkAuctionToken :: TokenName -> Value
    mkAuctionToken tokenName = Value.singleton auctionCs tokenName BigNum.one

    -- AuctionEscrow -------------------------------------------------

    auctionEscrowOref :: TransactionInput
    auctionEscrowOref = fst auctionEscrowUtxo

    auctionEscrowRedeemer :: RedeemerDatum
    auctionEscrowRedeemer = wrap $ toData StartBiddingRedeemer

    auctionEscrowDatum :: PlutusData
    auctionEscrowDatum = toData BiddingStarted

    auctionEscrowValue :: Value
    auctionEscrowValue = unsafePartial $ auctionLotValue <> mkAuctionToken
      auctionEscrowTokenName

    -- StandingBid ---------------------------------------------------

    standingBidDatum :: PlutusData
    standingBidDatum = toData $ StandingBidState Nothing

    standingBidTokenValue :: Value
    standingBidTokenValue = mkAuctionToken standingBidTokenName

    --

    txValidRange :: POSIXTimeRange
    txValidRange =
      mkFiniteInterval nowTime
        (auctionTermsRec.biddingEnd - wrap (BigInt.fromInt 1000))

    sellerPkh :: PaymentPubKeyHash
    sellerPkh = wrap $ unwrap $ unsafePartial fromJust $ toPubKeyHash
      auctionTermsRec.sellerAddress

    constraints :: TxConstraints
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

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton auctionEscrowOref $ snd auctionEscrowUtxo
      , Lookups.validator (unwrap validators).auctionEscrow
      ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

----------------------------------------------------------------------
-- Errors

data StartBiddingContractError
  = StartBidding_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | StartBidding_Error_CouldNotGetOwnAddress
  | StartBidding_Error_ContractNotInitiatedBySeller
  | StartBidding_Error_CurrentTimeBeforeBiddingStart
  | StartBidding_Error_CurrentTimeAfterBiddingEnd
  | StartBidding_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | StartBidding_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)
  | StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo
  | StartBidding_Error_AuctionLotValueConversionFailure

derive instance Generic StartBiddingContractError _
derive instance Eq StartBiddingContractError

instance Show StartBiddingContractError where
  show = genericShow

instance ToContractError StartBiddingContractError where
  errorCodePrefix = const "StartBidding"
  errorMessage = case _ of
    StartBidding_Error_InvalidAuctionTerms errors ->
      "Invalid auction terms, errors: " <> show errors <> "."

    StartBidding_Error_CouldNotGetOwnAddress ->
      "Could not get own address."

    StartBidding_Error_ContractNotInitiatedBySeller ->
      "Contract must be initiated by the seller."

    StartBidding_Error_CurrentTimeBeforeBiddingStart ->
      "Tx cannot be submitted before bidding start time."

    StartBidding_Error_CurrentTimeAfterBiddingEnd ->
      "Tx cannot be submitted after bidding end time."

    StartBidding_Error_CouldNotBuildAuctionValidators err ->
      "Could not build auction validators, error: " <> show err <> "."

    StartBidding_Error_InvalidAuctionInfo errors ->
      "Invalid auction info, errors: " <> show errors <> "."

    StartBidding_Error_CouldNotFindCurrentAuctionEscrowUtxo ->
      "Could not find current auction escrow utxo."

    StartBidding_Error_AuctionLotValueConversionFailure ->
      "Could not convert auction lot Plutus.Value to Cardano.Value."
