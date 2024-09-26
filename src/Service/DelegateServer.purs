module HydraAuctionOffchain.Service.DelegateServer
  ( getAvailableSlotsRequest
  , hostAuctionRequest
  , reserveSlotRequest
  ) where

import Prelude

import Affjax (Error, URL, Response, defaultRequest) as Affjax
import Affjax.RequestBody (RequestBody(Json)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.Helpers ((<</>>))
import Data.Argonaut (Json)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, encode) as CA
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(GET, POST))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Data.Set (Set)
import DelegateServer.AppManager.Types (AuctionSlot)
import DelegateServer.Handler.GetAvailableSlots (availableSlotsCodec)
import DelegateServer.Handlers.HostAuction
  ( HostAuctionError
  , HostAuctionRequest
  , hostAuctionRequestCodec
  , hostAuctionResponseCodec
  )
import DelegateServer.Handlers.ReserveSlot
  ( ReserveSlotError
  , ReserveSlotSuccess
  , ReserveSlotRequest
  , reserveSlotRequestCodec
  , reserveSlotResponseCodec
  )
import DelegateServer.Types.ServerResponse (ServerResponse)
import Effect.Aff (Aff)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import HydraAuctionOffchain.Service.Common
  ( ServiceError(ServiceDecodeJsonError, ServiceHttpError, ServiceHttpResponseError)
  )

getAvailableSlotsRequest :: String -> Aff (Either ServiceError (Set AuctionSlot))
getAvailableSlotsRequest httpServer =
  handleResponse availableSlotsCodec <$>
    getRequest (httpServer <</>> "availableSlots")

reserveSlotRequest
  :: String
  -> ReserveSlotRequest
  -> Aff (Either ServiceError (ServerResponse ReserveSlotSuccess ReserveSlotError))
reserveSlotRequest httpServer reqBody =
  handleResponse reserveSlotResponseCodec <$>
    postRequest
      (httpServer <</>> "reserveSlot")
      (CA.encode reserveSlotRequestCodec reqBody)

hostAuctionRequest
  :: String
  -> HostAuctionRequest
  -> Aff (Either ServiceError (ServerResponse Unit HostAuctionError))
hostAuctionRequest httpServer reqBody =
  handleResponse hostAuctionResponseCodec <$>
    postRequest
      (httpServer <</>> "hostAuction")
      (CA.encode hostAuctionRequestCodec reqBody)

getRequest :: Affjax.URL -> Aff (Either Affjax.Error (Affjax.Response String))
getRequest = affjaxRequest GET Nothing

postRequest :: Affjax.URL -> Json -> Aff (Either Affjax.Error (Affjax.Response String))
postRequest url content = affjaxRequest POST (Just content) url

affjaxRequest
  :: Method
  -> Maybe Json
  -> Affjax.URL
  -> Aff (Either Affjax.Error (Affjax.Response String))
affjaxRequest method content url =
  Affjax.request $ Affjax.defaultRequest
    { method = Left method
    , url = url
    , responseFormat = Affjax.ResponseFormat.string
    , content = Affjax.Json <$> content
    }

handleResponse
  :: forall (a :: Type)
   . CA.JsonCodec a
  -> Either Affjax.Error (Affjax.Response String)
  -> Either ServiceError a
handleResponse respCodec = case _ of
  Left affjaxErr ->
    Left $ ServiceHttpError $ wrap affjaxErr
  Right { status, body } ->
    case status of
      Affjax.StatusCode statusCode | statusCode >= 200 && statusCode <= 299 ->
        lmap (ServiceDecodeJsonError body) $
          caDecodeString respCodec body
      _ ->
        Left $ ServiceHttpResponseError status body
