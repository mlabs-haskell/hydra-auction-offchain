module HydraAuctionOffchain.Service.DelegateServer.Http
  ( getRequest
  , handleResponse
  , mkAuctionCsHeader
  , postRequest
  ) where

import Prelude

import Affjax (Error, Response, URL, defaultRequest) as Affjax
import Affjax.RequestBody (RequestBody(Json)) as Affjax
import Affjax.RequestHeader (RequestHeader(RequestHeader)) as Affjax
import Affjax.ResponseFormat (string) as Affjax.ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Cardano.AsCbor (encodeCbor)
import Cardano.Types (ScriptHash)
import Contract.CborBytes (cborBytesToHex)
import Ctl.Internal.Affjax (request) as Affjax
import Data.Argonaut (Json)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(GET, POST))
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import HydraAuctionOffchain.Service.Common
  ( ServiceError(ServiceDecodeJsonError, ServiceHttpError, ServiceHttpResponseError)
  )

mkAuctionCsHeader :: ScriptHash -> Affjax.RequestHeader
mkAuctionCsHeader =
  Affjax.RequestHeader "Auction-Cs"
    <<< cborBytesToHex
    <<< encodeCbor

getRequest :: Affjax.URL -> Aff (Either Affjax.Error (Affjax.Response String))
getRequest url =
  Affjax.request $ Affjax.defaultRequest
    { method = Left GET
    , url = url
    , responseFormat = Affjax.ResponseFormat.string
    }

postRequest
  :: { url :: Affjax.URL
     , content :: Maybe Json
     , headers :: Array Affjax.RequestHeader
     }
  -> Aff (Either Affjax.Error (Affjax.Response String))
postRequest { url, content, headers } =
  Affjax.request $ Affjax.defaultRequest
    { method = Left POST
    , url = url
    , headers = headers
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
