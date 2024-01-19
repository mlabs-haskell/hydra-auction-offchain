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
      )
  , PlaceBidContractParams(PlaceBidContractParams)
  , placeBidContract
  ) where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, OutputDatum(OutputDatum), Redeemer, fromData, toData)
import Contract.Prim.ByteArray (ByteArray)
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
import Contract.Value (Value)
import Contract.Value (flattenNonAdaAssets, singleton) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Error.Util ((!?))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (findMap) as Array
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Map (singleton, toUnfoldable) as Map
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Codec (class HasJson, bigIntCodec, byteArrayCodec)
import HydraAuctionOffchain.Contract.MintingPolicies (standingBidTokenName)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfo(AuctionInfo)
  , AuctionInfoValidationError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , BidTerms(BidTerms)
  , BidderInfo(BidderInfo)
  , ContractOutput
  , ContractResult
  , StandingBidRedeemer(NewBidRedeemer)
  , StandingBidState
  , Utxo
  , auctionInfoCodec
  , bidderSignatureMessage
  , emptySubmitTxData
  , mkContractOutput
  , submitTxReturningContractResult
  , validateAuctionInfo
  , validateAuctionTerms
  , validateNewBid
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Wallet (SignMessageError, signMessage)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt

newtype PlaceBidContractParams = PlaceBidContractParams
  { auctionInfo :: AuctionInfo
  , sellerSignature :: ByteArray
  , bidAmount :: BigInt
  }

derive instance Generic PlaceBidContractParams _
derive instance Newtype PlaceBidContractParams _
derive instance Eq PlaceBidContractParams

instance Show PlaceBidContractParams where
  show = genericShow

instance HasJson PlaceBidContractParams where
  jsonCodec = const placeBidContractParamsCodec

placeBidContractParamsCodec :: CA.JsonCodec PlaceBidContractParams
placeBidContractParamsCodec =
  wrapIso PlaceBidContractParams $ CA.object "PlaceBidContractParams" $
    CAR.record
      { auctionInfo: auctionInfoCodec
      , sellerSignature: byteArrayCodec
      , bidAmount: bigIntCodec
      }

placeBidContract :: PlaceBidContractParams -> Contract (ContractOutput TransactionHash)
placeBidContract =
  mkContractOutput _.txHash <<< mkPlaceBidContractWithErrors

mkPlaceBidContractWithErrors
  :: PlaceBidContractParams
  -> ExceptT PlaceBidContractError Contract ContractResult
mkPlaceBidContractWithErrors (PlaceBidContractParams params) = do
  let
    auctionInfo@(AuctionInfo auctionInfoRec) = params.auctionInfo
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
  validateAuctionInfo auctionInfo validators #
    validation (throwError <<< PlaceBid_Error_InvalidAuctionInfo) pure

  -- Query current standing bid utxo:
  standingBidUtxo /\ oldBidState <- queryStandingBidUtxo auctionInfo
    !? PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo

  -- Get bidder signature:
  bidderPkh <- ownPaymentPubKeyHash !? PlaceBid_Error_CouldNotGetOwnPubKeyHash
  let payload = bidderSignatureMessage auctionCs (unwrap bidderPkh) params.bidAmount
  { signature: bidderSignature, vkey: bidderVk, address: bidderAddress } <-
    withExceptT PlaceBid_Error_CouldNotSignBidderMessage $
      signMessage payload

  -- Check bid state transition:
  let
    newBidState :: StandingBidState
    newBidState = wrap $ Just $ BidTerms
      { bidder: BidderInfo { bidderAddress, bidderVk }
      , price: params.bidAmount
      , bidderSignature
      , sellerSignature: params.sellerSignature
      }
  success <- liftEffect $ validateNewBid auctionCs auctionTerms oldBidState newBidState
  unless success $ throwError PlaceBid_Error_InvalidBidStateTransition

  let
    validatorHashes = unwrap $ validatorHash <$> validators

    -- StandingBid ---------------------------------------------------

    standingBidOref :: TransactionInput
    standingBidOref = fst standingBidUtxo

    standingBidRedeemer :: Redeemer
    standingBidRedeemer = wrap $ toData NewBidRedeemer

    standingBidDatum :: Datum
    standingBidDatum = wrap $ toData newBidState

    standingBidTokenValue :: Value
    standingBidTokenValue = Value.singleton auctionCs standingBidTokenName one

    --

    txValidRange :: POSIXTimeRange
    txValidRange =
      mkFiniteInterval nowTime
        (auctionTermsRec.biddingEnd - wrap (BigInt.fromInt 1000))

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Spend standing bid utxo with old standing bid datum: 
        Constraints.mustSpendScriptOutput standingBidOref standingBidRedeemer

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
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton standingBidOref $ snd standingBidUtxo
      , Lookups.validator (unwrap validators).standingBid
      ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

queryStandingBidUtxo :: AuctionInfo -> Contract (Maybe (Utxo /\ StandingBidState))
queryStandingBidUtxo (AuctionInfo auctionInfo) =
  utxosAt auctionInfo.standingBidAddr
    <#> Array.findMap (\x -> Tuple x <$> currentStandingBidState (_.output $ unwrap $ snd x))
    <<< Map.toUnfoldable
  where
  currentStandingBidState :: TransactionOutput -> Maybe StandingBidState
  currentStandingBidState (TransactionOutput txOut)
    | Value.flattenNonAdaAssets txOut.amount /=
        [ auctionInfo.auctionId /\ standingBidTokenName /\ one ] = Nothing
    | isJust txOut.referenceScript = Nothing
    | otherwise =
        case txOut.datum of
          OutputDatum datum -> fromData $ unwrap datum
          _ -> Nothing

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

derive instance Generic PlaceBidContractError _
derive instance Eq PlaceBidContractError

instance Show PlaceBidContractError where
  show = genericShow

instance ToContractError PlaceBidContractError where
  toContractError = wrap <<< case _ of
    PlaceBid_Error_InvalidAuctionTerms validationErrors ->
      { errorCode: "PlaceBid01"
      , message: "Invalid auction terms, errors: " <> show validationErrors <> "."
      }
    PlaceBid_Error_CurrentTimeBeforeBiddingStart ->
      { errorCode: "PlaceBid02"
      , message: "Tx cannot be submitted before bidding start time."
      }
    PlaceBid_Error_CurrentTimeAfterBiddingEnd ->
      { errorCode: "PlaceBid03"
      , message: "Tx cannot be submitted after bidding end time."
      }
    PlaceBid_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "PlaceBid04"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
    PlaceBid_Error_InvalidAuctionInfo errors ->
      { errorCode: "PlaceBid05"
      , message: "Invalid auction info, errors: " <> show errors <> "."
      }
    PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo ->
      { errorCode: "PlaceBid06"
      , message: "Could not find current standing bid utxo."
      }
    PlaceBid_Error_CouldNotGetOwnPubKeyHash ->
      { errorCode: "PlaceBid07"
      , message: "Could not get own public key hash."
      }
    PlaceBid_Error_CouldNotSignBidderMessage err ->
      { errorCode: "PlaceBid08"
      , message: "Could not sign bidder message, error: " <> show err <> "."
      }
    PlaceBid_Error_InvalidBidStateTransition ->
      { errorCode: "PlaceBid09"
      , message: "Invalid bid state transition."
      }
