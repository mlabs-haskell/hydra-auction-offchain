module DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  , respCreatedOrBadRequest
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, stringify)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (class MonadAff)
import HTTPure (Response, badRequest, response) as HTTPure
import HTTPure.Status (created) as HTTPureStatus

data ServerResponse (success :: Type) (err :: Type)
  = ServerResponseSuccess success
  | ServerResponseError err

derive instance Generic (ServerResponse success err) _
derive instance (Eq success, Eq err) => Eq (ServerResponse success err)

instance (Show success, Show err) => Show (ServerResponse success err) where
  show = genericShow

respCreatedOrBadRequest
  :: forall m success err
   . MonadAff m
  => EncodeJson success
  => EncodeJson err
  => ServerResponse success err
  -> m HTTPure.Response
respCreatedOrBadRequest = case _ of
  ServerResponseSuccess success ->
    respCreated success
  ServerResponseError err ->
    respBadRequest err

respCreated :: forall m a. MonadAff m => EncodeJson a => a -> m HTTPure.Response
respCreated =
  HTTPure.response HTTPureStatus.created
    <<< stringify
    <<< encodeJson

respBadRequest :: forall m a. MonadAff m => EncodeJson a => a -> m HTTPure.Response
respBadRequest =
  HTTPure.badRequest
    <<< stringify
    <<< encodeJson
