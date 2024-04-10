module HydraAuctionOffchain.Contract.PlaceBidL2
  ( SendBidToDelegateContractError
      ( SendBidToDelegate_Error_MissingDelegateInfo
      , SendBidToDelegate_Error_InvalidAuctionTerms
      , SendBidToDelegate_Error_CurrentTimeBeforeBiddingStart
      , SendBidToDelegate_Error_CurrentTimeAfterBiddingEnd
      , SendBidToDelegate_Error_CouldNotBuildAuctionValidators
      , SendBidToDelegate_Error_InvalidAuctionInfo
      , SendBidToDelegate_Error_CouldNotGetOwnPubKeyHash
      , SendBidToDelegate_Error_CouldNotSignBidderMessage
      , SendBidToDelegate_Error_PlaceBidRequestServiceError
      )
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
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT(ExceptT), throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Affjax (request) as Affjax
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (encode) as CA
import Data.HTTP.Method (Method(POST))
import Data.Validation.Semigroup (validation)
import DelegateServer.Handlers.PlaceBid (PlaceBidResponse, placeBidResponseCodec)
import HydraAuctionOffchain.Contract.PlaceBid (PlaceBidContractParams(PlaceBidContractParams))
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfoExtended(AuctionInfoExtended)
  , AuctionInfoValidationError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , BidTerms(BidTerms)
  , BidderInfo(BidderInfo)
  , ContractOutput
  , bidTermsCodec
  , bidderSignatureMessage
  , mkContractOutput
  , randomHttpServer
  , validateAuctionInfo
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import HydraAuctionOffchain.Service.Common
  ( ServiceError(ServiceDecodeJsonError, ServiceHttpError, ServiceHttpResponseError)
  )
import HydraAuctionOffchain.Wallet (SignMessageError, signMessage)

sendBidContract :: PlaceBidContractParams -> Contract (ContractOutput PlaceBidResponse)
sendBidContract =
  mkContractOutput identity
    <<< mkSendBidContractWithErrors

mkSendBidContractWithErrors
  :: PlaceBidContractParams
  -> ExceptT SendBidToDelegateContractError Contract PlaceBidResponse
mkSendBidContractWithErrors (PlaceBidContractParams params) = do
  let
    (AuctionInfoExtended auctionInfoRec) = params.auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check delegate info:
  delegateInfo <- auctionInfoRec.delegateInfo
    ?? SendBidToDelegate_Error_MissingDelegateInfo

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< SendBidToDelegate_Error_InvalidAuctionTerms) pure

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError SendBidToDelegate_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError SendBidToDelegate_Error_CurrentTimeAfterBiddingEnd

  -- Build validators:
  validators <-
    withExceptT SendBidToDelegate_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  -- Check auction info:
  validateAuctionInfo auctionInfoRec validators #
    validation (throwError <<< SendBidToDelegate_Error_InvalidAuctionInfo) pure

  -- Get bidder signature:
  bidderPkh <- ownPaymentPubKeyHash !? SendBidToDelegate_Error_CouldNotGetOwnPubKeyHash
  let payload = bidderSignatureMessage auctionCs (unwrap bidderPkh) params.bidAmount
  { signature: bidderSignature, vkey: bidderVk, address: bidderAddress } <-
    withExceptT SendBidToDelegate_Error_CouldNotSignBidderMessage $
      signMessage payload

  -- Send `placeBid` requst to a randomly picked delegate:
  let
    bidTerms = BidTerms
      { bidder: BidderInfo { bidderAddress, bidderVk }
      , price: params.bidAmount
      , bidderSignature
      , sellerSignature: params.sellerSignature
      }

  delegateHttpServer <- randomHttpServer delegateInfo
  let sendBidToDelegate' = ExceptT $ liftAff $ sendBidToDelegate delegateHttpServer bidTerms
  withExceptT SendBidToDelegate_Error_PlaceBidRequestServiceError
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

data SendBidToDelegateContractError
  = SendBidToDelegate_Error_MissingDelegateInfo
  | SendBidToDelegate_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | SendBidToDelegate_Error_CurrentTimeBeforeBiddingStart
  | SendBidToDelegate_Error_CurrentTimeAfterBiddingEnd
  | SendBidToDelegate_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | SendBidToDelegate_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)
  | SendBidToDelegate_Error_CouldNotGetOwnPubKeyHash
  | SendBidToDelegate_Error_CouldNotSignBidderMessage SignMessageError
  | SendBidToDelegate_Error_PlaceBidRequestServiceError ServiceError

derive instance Generic SendBidToDelegateContractError _
derive instance Eq SendBidToDelegateContractError

instance Show SendBidToDelegateContractError where
  show = genericShow

instance ToContractError SendBidToDelegateContractError where
  toContractError = wrap <<< case _ of
    SendBidToDelegate_Error_MissingDelegateInfo ->
      { errorCode: "SendBidToDelegate01"
      , message: "Delegate info is missing."
      }
    SendBidToDelegate_Error_InvalidAuctionTerms validationErrors ->
      { errorCode: "SendBidToDelegate02"
      , message: "Invalid auction terms, errors: " <> show validationErrors <> "."
      }
    SendBidToDelegate_Error_CurrentTimeBeforeBiddingStart ->
      { errorCode: "SendBidToDelegate03"
      , message: "Tx cannot be submitted before bidding start time."
      }
    SendBidToDelegate_Error_CurrentTimeAfterBiddingEnd ->
      { errorCode: "SendBidToDelegate04"
      , message: "Tx cannot be submitted after bidding end time."
      }
    SendBidToDelegate_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "SendBidToDelegate05"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
    SendBidToDelegate_Error_InvalidAuctionInfo errors ->
      { errorCode: "SendBidToDelegate06"
      , message: "Invalid auction info, errors: " <> show errors <> "."
      }
    SendBidToDelegate_Error_CouldNotGetOwnPubKeyHash ->
      { errorCode: "SendBidToDelegate07"
      , message: "Could not get own public key hash."
      }
    SendBidToDelegate_Error_CouldNotSignBidderMessage err ->
      { errorCode: "SendBidToDelegate08"
      , message: "Could not sign bidder message, error: " <> show err <> "."
      }
    SendBidToDelegate_Error_PlaceBidRequestServiceError err ->
      { errorCode: "SendBidToDelegate09"
      , message: "Place bid request failed with error: " <> show err <> "."
      }

