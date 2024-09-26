module DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  , fromEither
  , respCreatedOrBadRequest
  , serverResponseCodec
  ) where

import Prelude

import Data.Argonaut (stringify)
import Data.Codec.Argonaut (JsonCodec, encode) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right), either)
import Data.Generic.Rep (class Generic)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Variant (inj, match) as Variant
import Effect.Aff.Class (class MonadAff)
import HTTPure (Response, badRequest, response) as HTTPure
import HTTPure.Status (created) as HTTPureStatus
import HydraAuctionOffchain.Lib.Codec (class HasJson, jsonCodec)
import Type.Proxy (Proxy(Proxy))

data ServerResponse (success :: Type) (err :: Type)
  = ServerResponseSuccess success
  | ServerResponseError err

derive instance Generic (ServerResponse success err) _
derive instance (Eq success, Eq err) => Eq (ServerResponse success err)

instance (Show success, Show err) => Show (ServerResponse success err) where
  show = genericShow

instance (HasJson success p, HasJson err p) => HasJson (ServerResponse success err) p where
  jsonCodec params _ =
    serverResponseCodec
      (jsonCodec params Proxy)
      (jsonCodec params Proxy)

fromEither :: forall success err. Either err success -> ServerResponse success err
fromEither = either ServerResponseError ServerResponseSuccess

serverResponseCodec
  :: forall success err
   . CA.JsonCodec success
  -> CA.JsonCodec err
  -> CA.JsonCodec (ServerResponse success err)
serverResponseCodec successCodec errCodec =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "Success": Right successCodec
        , "Error": Right errCodec
        }
    )
  where
  toVariant = case _ of
    ServerResponseSuccess x ->
      Variant.inj (Proxy :: _ "Success") x
    ServerResponseError x ->
      Variant.inj (Proxy :: _ "Error") x

  fromVariant = Variant.match
    { "Success": ServerResponseSuccess
    , "Error": ServerResponseError
    }

respCreatedOrBadRequest
  :: forall m success err
   . MonadAff m
  => CA.JsonCodec (ServerResponse success err)
  -> ServerResponse success err
  -> m HTTPure.Response
respCreatedOrBadRequest respCodec resp =
  case resp of
    ServerResponseSuccess _ ->
      HTTPure.response HTTPureStatus.created respBody
    ServerResponseError _ ->
      HTTPure.badRequest respBody
  where
  respBody :: String
  respBody = stringify $ CA.encode respCodec resp
