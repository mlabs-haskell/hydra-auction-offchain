module HydraAuctionOffchain.Contract.SendBid
  ( SendBidContractError
      ( SendBid_Error_InvalidAuctionTerms
      , SendBid_Error_InvalidDelegateInfo
      , SendBid_Error_CurrentTimeBeforeBiddingStart
      , SendBid_Error_CurrentTimeAfterBiddingEnd
      , SendBid_Error_CouldNotGetOwnPubKeyHash
      , SendBid_Error_CouldNotSignBidderMessage
      , SendBid_Error_PlaceBidRequestServiceError
      )
  , SendBidContractParams(SendBidContractParams)
  , sendBidContract
  , mkSendBidContractWithErrors
  ) where

import Contract.Prelude

import Affjax (Error, Response, defaultRequest) as Affjax
import Affjax.RequestBody (RequestBody(Json)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol)
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Error.Util ((!?))
import Control.Monad.Except (ExceptT(ExceptT), throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Affjax (request) as Affjax
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, encode, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.HTTP.Method (Method(POST))
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import DelegateServer.Handlers.PlaceBid (PlaceBidResponse, placeBidResponseCodec)
import HydraAuctionOffchain.Codec
  ( class HasJson
  , bigIntCodec
  , byteArrayCodec
  , currencySymbolCodec
  )
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , BidTerms(BidTerms)
  , BidderInfo(BidderInfo)
  , ContractOutput
  , DelegateInfo
  , DelegateInfoValidationError
  , auctionTermsCodec
  , bidTermsCodec
  , bidderSignatureMessage
  , delegateInfoCodec
  , mkContractOutput
  , randomHttpServer
  , validateAuctionTerms
  , validateDelegateInfo
  )
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import HydraAuctionOffchain.Service.Common
  ( ServiceError(ServiceDecodeJsonError, ServiceHttpError, ServiceHttpResponseError)
  )
import HydraAuctionOffchain.Wallet (SignMessageError, signMessage)
import JS.BigInt (BigInt)

newtype SendBidContractParams = SendBidContractParams
  { auctionCs :: CurrencySymbol
  , auctionTerms :: AuctionTerms
  , delegateInfo :: DelegateInfo
  , sellerSignature :: ByteArray
  , bidAmount :: BigInt
  }

derive instance Generic SendBidContractParams _
derive instance Newtype SendBidContractParams _
derive instance Eq SendBidContractParams

instance Show SendBidContractParams where
  show = genericShow

instance HasJson SendBidContractParams where
  jsonCodec = const sendBidContractParamsCodec

sendBidContractParamsCodec :: CA.JsonCodec SendBidContractParams
sendBidContractParamsCodec =
  wrapIso SendBidContractParams $ CA.object "SendBidContractParams" $
    CAR.record
      { auctionCs: currencySymbolCodec
      , auctionTerms: auctionTermsCodec
      , delegateInfo: delegateInfoCodec
      , sellerSignature: byteArrayCodec
      , bidAmount: bigIntCodec
      }

sendBidContract :: SendBidContractParams -> Contract (ContractOutput PlaceBidResponse)
sendBidContract =
  mkContractOutput identity
    <<< mkSendBidContractWithErrors

mkSendBidContractWithErrors
  :: SendBidContractParams
  -> ExceptT SendBidContractError Contract PlaceBidResponse
mkSendBidContractWithErrors (SendBidContractParams params) = do
  let
    auctionCs = params.auctionCs
    auctionTerms@(AuctionTerms auctionTermsRec) = params.auctionTerms
    delegateInfo = params.delegateInfo

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< SendBid_Error_InvalidAuctionTerms) pure

  -- Check delegate info:
  validateDelegateInfo delegateInfo auctionTerms #
    validation (throwError <<< SendBid_Error_InvalidDelegateInfo) pure

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError SendBid_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError SendBid_Error_CurrentTimeAfterBiddingEnd

  -- Get bidder signature:
  bidderPkh <- ownPaymentPubKeyHash !? SendBid_Error_CouldNotGetOwnPubKeyHash
  let payload = bidderSignatureMessage auctionCs (unwrap bidderPkh) params.bidAmount
  { signature: bidderSignature, vkey: bidderVk, address: bidderAddress } <-
    withExceptT SendBid_Error_CouldNotSignBidderMessage $
      signMessage payload

  -- Send `placeBid` request to a randomly picked delegate:
  let
    bidTerms = BidTerms
      { bidder: BidderInfo { bidderAddress, bidderVk }
      , price: params.bidAmount
      , bidderSignature
      , sellerSignature: params.sellerSignature
      }

  delegateHttpServer <- randomHttpServer delegateInfo
  let sendBidToDelegate' = ExceptT $ liftAff $ sendBidToDelegate delegateHttpServer bidTerms
  withExceptT SendBid_Error_PlaceBidRequestServiceError
    sendBidToDelegate'

sendBidToDelegate :: String -> BidTerms -> Aff (Either ServiceError PlaceBidResponse)
sendBidToDelegate httpServer bidTerms = do
  handleResponse <$> Affjax.request
    ( Affjax.defaultRequest
        { method = Left POST
        , url = httpServer <> "/placeBid"
        , content = Just $ Affjax.Json $ CA.encode bidTermsCodec bidTerms
        , responseFormat = Affjax.ResponseFormat.string
        }
    )

handleResponse
  :: Either Affjax.Error (Affjax.Response String)
  -> Either ServiceError PlaceBidResponse
handleResponse = case _ of
  Left affjaxError ->
    Left $ ServiceHttpError $ wrap affjaxError
  Right { status, body } ->
    case status of
      Affjax.StatusCode statusCode | statusCode == 201 || statusCode == 400 ->
        lmap (ServiceDecodeJsonError body) $
          caDecodeString placeBidResponseCodec body
      _ ->
        Left $ ServiceHttpResponseError status body

----------------------------------------------------------------------
-- Errors

data SendBidContractError
  = SendBid_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | SendBid_Error_InvalidDelegateInfo (Array DelegateInfoValidationError)
  | SendBid_Error_CurrentTimeBeforeBiddingStart
  | SendBid_Error_CurrentTimeAfterBiddingEnd
  | SendBid_Error_CouldNotGetOwnPubKeyHash
  | SendBid_Error_CouldNotSignBidderMessage SignMessageError
  | SendBid_Error_PlaceBidRequestServiceError ServiceError

derive instance Generic SendBidContractError _
derive instance Eq SendBidContractError

instance Show SendBidContractError where
  show = genericShow

instance ToContractError SendBidContractError where
  errorCodePrefix = const "SendBid"
  errorMessage = case _ of
    SendBid_Error_InvalidAuctionTerms validationErrors ->
      "Invalid auction terms, errors: " <> show validationErrors <> "."

    SendBid_Error_InvalidDelegateInfo validationErrors ->
      "Invalid delegate info, errors: " <> show validationErrors <> "."

    SendBid_Error_CurrentTimeBeforeBiddingStart ->
      "Tx cannot be submitted before bidding start time."

    SendBid_Error_CurrentTimeAfterBiddingEnd ->
      "Tx cannot be submitted after bidding end time."

    SendBid_Error_CouldNotGetOwnPubKeyHash ->
      "Could not get own public key hash."

    SendBid_Error_CouldNotSignBidderMessage err ->
      "Could not sign bidder message, error: " <> show err <> "."

    SendBid_Error_PlaceBidRequestServiceError err ->
      "PlaceBid request failed with error: " <> show err <> "."
