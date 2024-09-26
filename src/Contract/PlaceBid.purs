module HydraAuctionOffchain.Contract.PlaceBid
  ( PlaceBidContractError
      ( PlaceBid_Error_InvalidAuctionTerms
      , PlaceBid_Error_CurrentTimeBeforeBiddingStart
      , PlaceBid_Error_CurrentTimeAfterBiddingEnd
      , PlaceBid_Error_CouldNotBuildAuctionValidators
      , PlaceBid_Error_InvalidAuctionInfo
      , PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo
      , PlaceBid_Error_CouldNotGetOwnPubKeyHash
      , PlaceBid_Error_CouldNotSignBidderMessage
      , PlaceBid_Error_InvalidBidStateTransition
      , PlaceBid_Error_MissingMetadataOref
      , PlaceBid_Error_CouldNotQueryAuctionMetadataUtxo
      , PlaceBid_Error_BidderAddressConversionFailure
      )
  , PlaceBidContractParams(PlaceBidContractParams)
  , mkPlaceBidContractWithErrors
  , placeBidContract
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address (fromCardano) as Plutus.Address
import Cardano.Types
  ( BigNum
  , NetworkId
  , PlutusData
  , RedeemerDatum
  , TransactionHash
  , TransactionInput
  , TransactionUnspentOutput
  )
import Cardano.Types.BigNum (one) as BigNum
import Contract.Address (getNetworkId)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTimeRange, mkFiniteInterval)
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutputUsingScriptRef
  , mustValidateIn
  ) as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (Value)
import Contract.Value (singleton) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Map (fromFoldable) as Map
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Codec (bigNumCodec, byteArrayCodec)
import HydraAuctionOffchain.Contract.MintingPolicies (standingBidTokenName)
import HydraAuctionOffchain.Contract.QueryUtxo (queryStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfoExtended(AuctionInfoExtended)
  , AuctionInfoValidationError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , BidTerms(BidTerms)
  , BidderInfo(BidderInfo)
  , ContractOutput
  , ContractResult
  , StandingBidRedeemer(NewBidRedeemer)
  , StandingBidState
  , auctionInfoExtendedCodec
  , bidderSignatureMessage
  , emptySubmitTxData
  , mkContractOutput
  , submitTxReturningContractResult
  , validateAuctionInfo
  , validateAuctionTerms
  , validateNewBid
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Wallet (SignMessageError, signMessage)
import JS.BigInt (fromInt) as BigInt

newtype PlaceBidContractParams = PlaceBidContractParams
  { auctionInfo :: AuctionInfoExtended
  , sellerSignature :: ByteArray
  , bidAmount :: BigNum
  }

derive instance Generic PlaceBidContractParams _
derive instance Newtype PlaceBidContractParams _
derive instance Eq PlaceBidContractParams

instance Show PlaceBidContractParams where
  show = genericShow

instance HasJson PlaceBidContractParams NetworkId where
  jsonCodec network = const (placeBidContractParamsCodec network)

placeBidContractParamsCodec :: NetworkId -> CA.JsonCodec PlaceBidContractParams
placeBidContractParamsCodec network =
  wrapIso PlaceBidContractParams $ CA.object "PlaceBidContractParams" $
    CAR.record
      { auctionInfo: auctionInfoExtendedCodec network
      , sellerSignature: byteArrayCodec
      , bidAmount: bigNumCodec
      }

placeBidContract :: PlaceBidContractParams -> Contract (ContractOutput TransactionHash)
placeBidContract =
  mkContractOutput _.txHash <<< mkPlaceBidContractWithErrors

mkPlaceBidContractWithErrors
  :: PlaceBidContractParams
  -> ExceptT PlaceBidContractError Contract ContractResult
mkPlaceBidContractWithErrors (PlaceBidContractParams params) = do
  let
    (AuctionInfoExtended auctionInfoRec) = params.auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< PlaceBid_Error_InvalidAuctionTerms) pure

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError PlaceBid_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError PlaceBid_Error_CurrentTimeAfterBiddingEnd

  -- Build validators:
  validators <-
    withExceptT PlaceBid_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  -- Check auction info:
  validateAuctionInfo auctionInfoRec validators #
    validation (throwError <<< PlaceBid_Error_InvalidAuctionInfo) pure

  -- Query auction metadata utxo:
  auctionMetadataOref <- auctionInfoRec.metadataOref ?? PlaceBid_Error_MissingMetadataOref
  auctionMetadataTxOut <- getUtxo auctionMetadataOref
    !? PlaceBid_Error_CouldNotQueryAuctionMetadataUtxo

  -- Query current standing bid utxo:
  standingBidUtxo /\ oldBidState <- queryStandingBidUtxo auctionInfoRec
    !? PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo

  -- Get bidder signature:
  bidderPkh <- ownPaymentPubKeyHash !? PlaceBid_Error_CouldNotGetOwnPubKeyHash
  let payload = bidderSignatureMessage auctionCs (unwrap bidderPkh) params.bidAmount
  { signature: bidderSignature, vkey: bidderVk, address: bidderAddress } <-
    withExceptT PlaceBid_Error_CouldNotSignBidderMessage $
      signMessage payload

  -- Convert bidder address:
  bidderAddressPlutus <- Plutus.Address.fromCardano bidderAddress
    ?? PlaceBid_Error_BidderAddressConversionFailure

  -- Check bid state transition:
  let
    newBidState :: StandingBidState
    newBidState = wrap $ Just $ BidTerms
      { bidder: BidderInfo { bidderAddress: bidderAddressPlutus, bidderVk }
      , price: params.bidAmount
      , bidderSignature
      , sellerSignature: params.sellerSignature
      }
  network <- lift getNetworkId
  success <- liftEffect $ validateNewBid network auctionCs auctionTerms oldBidState newBidState
  unless success $ throwError PlaceBid_Error_InvalidBidStateTransition

  let
    validatorHashes = unwrap $ validatorHash <$> validators

    -- StandingBid ---------------------------------------------------

    standingBidOref :: TransactionInput
    standingBidOref = fst standingBidUtxo

    standingBidRedeemer :: RedeemerDatum
    standingBidRedeemer = wrap $ toData NewBidRedeemer

    standingBidDatum :: PlutusData
    standingBidDatum = toData newBidState

    standingBidTokenValue :: Value
    standingBidTokenValue = Value.singleton auctionCs standingBidTokenName BigNum.one

    -- AuctionMetadata -----------------------------------------------

    auctionMetadataUtxo :: TransactionUnspentOutput
    auctionMetadataUtxo = wrap { input: auctionMetadataOref, output: auctionMetadataTxOut }

    --

    txValidRange :: POSIXTimeRange
    txValidRange =
      mkFiniteInterval nowTime
        (auctionTermsRec.biddingEnd - wrap (BigInt.fromInt 1000))

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Spend standing bid utxo with old standing bid datum: 
        Constraints.mustSpendScriptOutputUsingScriptRef standingBidOref standingBidRedeemer
          (RefInput auctionMetadataUtxo)

      , -- Lock standing bid token with new standing bid datum at
        -- standing bid validator address:
        Constraints.mustPayToScript validatorHashes.standingBid standingBidDatum DatumInline
          standingBidTokenValue

      , -- This transaction must be signed by the bidder:
        Constraints.mustBeSignedBy bidderPkh

      , -- Set transaction validity interval to bidding period: 
        Constraints.mustValidateIn txValidRange
      ]

    lookups :: ScriptLookups
    lookups = Lookups.unspentOutputs $ Map.fromFoldable [ standingBidUtxo ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

----------------------------------------------------------------------
-- Errors

data PlaceBidContractError
  = PlaceBid_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | PlaceBid_Error_CurrentTimeBeforeBiddingStart
  | PlaceBid_Error_CurrentTimeAfterBiddingEnd
  | PlaceBid_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | PlaceBid_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)
  | PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo
  | PlaceBid_Error_CouldNotGetOwnPubKeyHash
  | PlaceBid_Error_CouldNotSignBidderMessage SignMessageError
  | PlaceBid_Error_InvalidBidStateTransition
  | PlaceBid_Error_MissingMetadataOref
  | PlaceBid_Error_CouldNotQueryAuctionMetadataUtxo
  | PlaceBid_Error_BidderAddressConversionFailure

derive instance Generic PlaceBidContractError _
derive instance Eq PlaceBidContractError

instance Show PlaceBidContractError where
  show = genericShow

instance ToContractError PlaceBidContractError where
  errorCodePrefix = const "PlaceBid"
  errorMessage = case _ of
    PlaceBid_Error_InvalidAuctionTerms validationErrors ->
      "Invalid auction terms, errors: " <> show validationErrors <> "."

    PlaceBid_Error_CurrentTimeBeforeBiddingStart ->
      "Tx cannot be submitted before bidding start time."

    PlaceBid_Error_CurrentTimeAfterBiddingEnd ->
      "Tx cannot be submitted after bidding end time."

    PlaceBid_Error_CouldNotBuildAuctionValidators err ->
      "Could not build auction validators, error: " <> show err <> "."

    PlaceBid_Error_InvalidAuctionInfo errors ->
      "Invalid auction info, errors: " <> show errors <> "."

    PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo ->
      "Could not find current standing bid utxo."

    PlaceBid_Error_CouldNotGetOwnPubKeyHash ->
      "Could not get own public key hash."

    PlaceBid_Error_CouldNotSignBidderMessage err ->
      "Could not sign bidder message, error: " <> show err <> "."

    PlaceBid_Error_InvalidBidStateTransition ->
      "Invalid bid state transition."

    PlaceBid_Error_MissingMetadataOref ->
      "Auction metadata output reference not provided."

    PlaceBid_Error_CouldNotQueryAuctionMetadataUtxo ->
      "Could not query auction metadata utxo."

    PlaceBid_Error_BidderAddressConversionFailure ->
      "Could not convert bidder address to Plutus.Address."
