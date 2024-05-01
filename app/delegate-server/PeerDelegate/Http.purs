module DelegateServer.PeerDelegate.Http
  ( signCommitTx
  ) where

import Prelude

import Affjax (Error, Response, URL, defaultRequest) as Affjax
import Affjax.RequestBody (RequestBody(Json)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Contract.Config (ServerConfig)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.ServerConfig (mkHttpUrl)
import Data.Argonaut (Json)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import DelegateServer.Handlers.SignCommitTx
  ( SignCommitTxRequestPayload
  , SignCommitTxResponse
  , signCommitTxRequestPayloadCodec
  , signCommitTxResponseCodec
  )
import Effect.Aff (Aff)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import HydraAuctionOffchain.Service.Common
  ( ServiceError(ServiceDecodeJsonError, ServiceHttpError, ServiceHttpResponseError)
  )

signCommitTx
  :: ServerConfig
  -> SignCommitTxRequestPayload
  -> Aff (Either ServiceError SignCommitTxResponse)
signCommitTx serverConfig reqPayload = do
  let endpoint = mkHttpUrl serverConfig <> "/signCommitTx"
  handleResponse <$>
    postRequest endpoint
      (Just $ CA.encode signCommitTxRequestPayloadCodec reqPayload)

postRequest :: Affjax.URL -> Maybe Json -> Aff (Either Affjax.Error (Affjax.Response String))
postRequest endpoint mPayload =
  Affjax.request $ Affjax.defaultRequest
    { method = Left POST
    , url = endpoint
    , content = Affjax.Json <$> mPayload
    , responseFormat = Affjax.ResponseFormat.string
    }

handleResponse
  :: Either Affjax.Error (Affjax.Response String)
  -> Either ServiceError SignCommitTxResponse
handleResponse = case _ of
  Left affjaxError ->
    Left $ ServiceHttpError $ wrap affjaxError
  Right { status, body } ->
    case status of
      Affjax.StatusCode statusCode | statusCode /= 201 && statusCode /= 400 ->
        Left $ ServiceHttpResponseError status body
      _ ->
        lmap (ServiceDecodeJsonError body) $
          caDecodeString signCommitTxResponseCodec body
