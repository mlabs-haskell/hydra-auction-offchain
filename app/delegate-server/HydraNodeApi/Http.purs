module DelegateServer.HydraNodeApi.Http
  ( commit
  ) where

import Prelude

import Affjax (Error, Response, URL, defaultRequest) as Affjax
import Affjax.RequestBody (RequestBody(Json)) as Affjax
import Affjax.ResponseFormat (json) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Contract.Config (ServerConfig)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.ServerConfig (mkHttpUrl)
import Data.Argonaut (Json, encodeJson)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, decode) as CA
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(Just))
import Data.Newtype (wrap)
import DelegateServer.HydraNodeApi.Types.Commit (CommitUtxoMap)
import DelegateServer.HydraNodeApi.Types.DraftCommitTx (DraftCommitTx, draftCommitTxCodec)
import DelegateServer.Types.ServiceError
  ( ServiceError(ServiceDecodeJsonError, ServiceHttpError, ServiceHttpResponseError)
  )
import Effect.Aff (Aff)

commit :: ServerConfig -> CommitUtxoMap -> Aff (Either ServiceError DraftCommitTx)
commit serverConfig utxos = do
  let endpoint = mkHttpUrl serverConfig <> "/commit"
  handleResponse draftCommitTxCodec <$>
    postRequest endpoint (Just $ encodeJson utxos)

postRequest :: Affjax.URL -> Maybe Json -> Aff (Either Affjax.Error (Affjax.Response Json))
postRequest endpoint mPayload =
  Affjax.request $ Affjax.defaultRequest
    { method = Left POST
    , url = endpoint
    , content = Affjax.Json <$> mPayload
    , responseFormat = Affjax.ResponseFormat.json
    }

handleResponse
  :: forall (a :: Type)
   . CA.JsonCodec a
  -> Either Affjax.Error (Affjax.Response Json)
  -> Either ServiceError a
handleResponse codec = case _ of
  Left affjaxError ->
    Left $ ServiceHttpError $ wrap affjaxError
  Right { status, body } ->
    case status of
      Affjax.StatusCode statusCode | statusCode < 200 || statusCode > 299 ->
        Left $ ServiceHttpResponseError status $ wrap body
      _ ->
        CA.decode codec body
          # lmap (ServiceDecodeJsonError (wrap body))
