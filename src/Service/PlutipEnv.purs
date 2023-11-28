module HydraAuctionOffchain.Service.PlutipEnv
  ( queryPlutipEnvPrivateKey
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeJsonString)
import Affjax (Error, Response, defaultRequest) as Affjax
import Affjax.ResponseFormat (string) as Affjax
import Affjax.StatusCode (StatusCode(StatusCode)) as Affjax
import Contract.Config (ServerConfig)
import Contract.Wallet (PrivatePaymentKey)
import Ctl.Internal.Affjax (request) as Affjax
import Ctl.Internal.ServerConfig (mkHttpUrl)
import Data.Argonaut (JsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(GET))
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import HydraAuctionOffchain.Helpers (liftEitherShow)
import HydraAuctionOffchain.Service.Common (AffjaxError)

data PlutipEnvError
  = PlutipEnvDecodeJsonError String JsonDecodeError
  | PlutipEnvHttpError AffjaxError
  | PlutipEnvHttpResponseError Affjax.StatusCode String

derive instance Generic PlutipEnvError _

instance Show PlutipEnvError where
  show = genericShow

queryPlutipEnvPrivateKey :: ServerConfig -> Aff PrivatePaymentKey
queryPlutipEnvPrivateKey serverConfig = do
  resp <- Affjax.request $ Affjax.defaultRequest
    { method = Left GET
    , url = mkHttpUrl serverConfig <> "/skey"
    , responseFormat = Affjax.string
    }
  liftEitherShow $ handleResponse resp

handleResponse
  :: forall (a :: Type)
   . DecodeAeson a
  => Either Affjax.Error (Affjax.Response String)
  -> Either PlutipEnvError a
handleResponse (Left affjaxError) =
  Left $ PlutipEnvHttpError $ wrap affjaxError
handleResponse (Right { status: statusCode, body })
  | statusCode < Affjax.StatusCode 200 || statusCode > Affjax.StatusCode 299 =
      Left $ PlutipEnvHttpResponseError statusCode body
  | otherwise =
      decodeJsonString body # lmap (PlutipEnvDecodeJsonError body)
