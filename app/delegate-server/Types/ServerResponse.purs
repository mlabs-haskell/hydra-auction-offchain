module DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  , respCreatedOrBadRequest
  , serverResponseCodec
  ) where

import Prelude

import Data.Argonaut (stringify)
import Data.Codec.Argonaut (JsonCodec, encode) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right))
import Data.Generic.Rep (class Generic)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Variant (inj, match) as Variant
import Effect.Aff.Class (class MonadAff)
import HTTPure (Response, badRequest, response) as HTTPure
import HTTPure.Status (created) as HTTPureStatus
import HydraAuctionOffchain.Codec (class HasJson, jsonCodec)
import Type.Proxy (Proxy(Proxy))

data ServerResponse (success :: Type) (err :: Type)
  = ServerResponseSuccess success
  | ServerResponseError err

derive instance Generic (ServerResponse success err) _
derive instance (Eq success, Eq err) => Eq (ServerResponse success err)

instance (Show success, Show err) => Show (ServerResponse success err) where
  show = genericShow

instance (HasJson success, HasJson err) => HasJson (ServerResponse success err) where
  jsonCodec = const (serverResponseCodec (jsonCodec Proxy) (jsonCodec Proxy))

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
  => HasJson (ServerResponse success err)
  => ServerResponse success err
  -> m HTTPure.Response
respCreatedOrBadRequest resp =
  case resp of
    ServerResponseSuccess _ ->
      HTTPure.response HTTPureStatus.created respBody
    ServerResponseError _ ->
      HTTPure.badRequest respBody
  where
  respBody :: String
  respBody = stringify $ CA.encode (jsonCodec Proxy) resp
