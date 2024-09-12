module HydraAuctionOffchain.Contract.MoveBid
  ( MoveBidContractError
      ( MoveBid_Error_InvalidAuctionTerms
      , MoveBid_Error_InvalidDelegateInfo
      , MoveBid_Error_CurrentTimeBeforeBiddingStart
      , MoveBid_Error_CurrentTimeAfterBiddingEnd
      , MoveBid_Error_MoveBidRequestServiceError
      )
  , MoveBidContractParams(MoveBidContractParams)
  , moveBidContract
  , mkMoveBidContractWithErrors
  ) where

import Contract.Prelude

import Affjax (Error, Response, defaultRequest) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Cardano.Types (NetworkId, ScriptHash)
import Contract.Address (getNetworkId)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Control.Monad.Except (ExceptT(ExceptT), throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Affjax (request) as Affjax
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.HTTP.Method (Method(POST))
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import DelegateServer.Handlers.MoveBid (MoveBidResponse, moveBidResponseCodec)
import HydraAuctionOffchain.Codec (scriptHashCodec)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , ContractOutput
  , DelegateInfo
  , DelegateInfoValidationError
  , auctionTermsCodec
  , delegateInfoCodec
  , mkContractOutput
  , randomHttpServer
  , validateAuctionTerms
  , validateDelegateInfo
  )
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import HydraAuctionOffchain.Service.Common
  ( ServiceError(ServiceDecodeJsonError, ServiceHttpError, ServiceHttpResponseError)
  )

newtype MoveBidContractParams = MoveBidContractParams
  { auctionCs :: ScriptHash
  , auctionTerms :: AuctionTerms
  , delegateInfo :: DelegateInfo
  }

derive instance Generic MoveBidContractParams _
derive instance Newtype MoveBidContractParams _
derive instance Eq MoveBidContractParams

instance Show MoveBidContractParams where
  show = genericShow

instance HasJson MoveBidContractParams NetworkId where
  jsonCodec network = const (moveBidContractParamsCodec network)

moveBidContractParamsCodec :: NetworkId -> CA.JsonCodec MoveBidContractParams
moveBidContractParamsCodec network =
  wrapIso MoveBidContractParams $ CA.object "MoveBidContractParams" $
    CAR.record
      { auctionCs: scriptHashCodec
      , auctionTerms: auctionTermsCodec network
      , delegateInfo: delegateInfoCodec
      }

moveBidContract :: MoveBidContractParams -> Contract (ContractOutput MoveBidResponse)
moveBidContract =
  mkContractOutput identity
    <<< mkMoveBidContractWithErrors

mkMoveBidContractWithErrors
  :: MoveBidContractParams
  -> ExceptT MoveBidContractError Contract MoveBidResponse
mkMoveBidContractWithErrors (MoveBidContractParams params) = do
  let
    _auctionCs = params.auctionCs
    auctionTerms@(AuctionTerms auctionTermsRec) = params.auctionTerms
    delegateInfo = params.delegateInfo

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< MoveBid_Error_InvalidAuctionTerms) pure

  -- Check delegate info:
  validateDelegateInfo delegateInfo auctionTerms #
    validation (throwError <<< MoveBid_Error_InvalidDelegateInfo) pure

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError MoveBid_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError MoveBid_Error_CurrentTimeAfterBiddingEnd

  -- Send `moveBid` request to a randomly picked delegate:
  delegateHttpServer <- randomHttpServer delegateInfo
  network <- lift getNetworkId
  let moveBidRequest' = ExceptT $ liftAff $ moveBidRequest network delegateHttpServer
  withExceptT MoveBid_Error_MoveBidRequestServiceError
    moveBidRequest'

moveBidRequest :: NetworkId -> String -> Aff (Either ServiceError MoveBidResponse)
moveBidRequest network httpServer = do
  handleResponse network <$> Affjax.request
    ( Affjax.defaultRequest
        { method = Left POST
        , url = httpServer <> "/moveBid"
        , responseFormat = Affjax.ResponseFormat.string
        }
    )

handleResponse
  :: NetworkId
  -> Either Affjax.Error (Affjax.Response String)
  -> Either ServiceError MoveBidResponse
handleResponse network = case _ of
  Left affjaxError ->
    Left $ ServiceHttpError $ wrap affjaxError
  Right { status, body } ->
    case status of
      Affjax.StatusCode statusCode | statusCode == 201 || statusCode == 400 ->
        lmap (ServiceDecodeJsonError body) $
          caDecodeString (moveBidResponseCodec network) body
      _ ->
        Left $ ServiceHttpResponseError status body

----------------------------------------------------------------------
-- Errors

data MoveBidContractError
  = MoveBid_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | MoveBid_Error_InvalidDelegateInfo (Array DelegateInfoValidationError)
  | MoveBid_Error_CurrentTimeBeforeBiddingStart
  | MoveBid_Error_CurrentTimeAfterBiddingEnd
  | MoveBid_Error_MoveBidRequestServiceError ServiceError

derive instance Generic MoveBidContractError _

instance Show MoveBidContractError where
  show = genericShow

instance ToContractError MoveBidContractError where
  errorCodePrefix = const "MoveBid"
  errorMessage = case _ of
    MoveBid_Error_InvalidAuctionTerms validationErrors ->
      "Invalid auction terms, errors: " <> show validationErrors <> "."

    MoveBid_Error_InvalidDelegateInfo validationErrors ->
      "Invalid delegate info, errors: " <> show validationErrors <> "."

    MoveBid_Error_CurrentTimeBeforeBiddingStart ->
      "Tx cannot be submitted before bidding start time."

    MoveBid_Error_CurrentTimeAfterBiddingEnd ->
      "Tx cannot be submitted after bidding end time."

    MoveBid_Error_MoveBidRequestServiceError err ->
      "MoveBid request failed with error: " <> show err <> "."
