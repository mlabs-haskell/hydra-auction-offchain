module DelegateServer.HydraNodeApi.Http
  ( commit
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
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import DelegateServer.Types.HydraDraftCommitTx (DraftCommitTx, draftCommitTxCodec)
import Effect.Aff (Aff)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import HydraAuctionOffchain.Service.Common
  ( ServiceError(ServiceDecodeJsonError, ServiceHttpError, ServiceHttpResponseError)
  )

commit :: ServerConfig -> Json -> Aff (Either ServiceError DraftCommitTx)
commit serverConfig commitRequest = do
  let endpoint = mkHttpUrl serverConfig <> "/commit"
  handleResponse draftCommitTxCodec <$>
    postRequest endpoint (Just commitRequest)

postRequest :: Affjax.URL -> Maybe Json -> Aff (Either Affjax.Error (Affjax.Response String))
postRequest endpoint mPayload =
  Affjax.request $ Affjax.defaultRequest
    { method = Left POST
    , url = endpoint
    , content = Affjax.Json <$> mPayload
    , responseFormat = Affjax.ResponseFormat.string
    }

handleResponse
  :: forall (a :: Type)
   . CA.JsonCodec a
  -> Either Affjax.Error (Affjax.Response String)
  -> Either ServiceError a
handleResponse codec = case _ of
  Left affjaxError ->
    Left $ ServiceHttpError $ wrap affjaxError
  Right { status, body } ->
    case status of
      Affjax.StatusCode statusCode | statusCode < 200 || statusCode > 299 ->
        Left $ ServiceHttpResponseError status body
      _ ->
        caDecodeString codec body
          # lmap (ServiceDecodeJsonError body)
