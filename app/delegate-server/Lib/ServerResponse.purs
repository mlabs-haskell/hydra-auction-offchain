module DelegateServer.Lib.ServerResponse
  ( respBadRequest
  , respCreated
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, stringify)
import Effect.Aff.Class (class MonadAff)
import HTTPure (Response, badRequest, response) as HTTPure
import HTTPure.Status (created) as HTTPureStatus

respBadRequest :: forall m a. MonadAff m => EncodeJson a => a -> m HTTPure.Response
respBadRequest =
  HTTPure.badRequest
    <<< stringify
    <<< encodeJson

respCreated :: forall m a. MonadAff m => EncodeJson a => a -> m HTTPure.Response
respCreated =
  HTTPure.response HTTPureStatus.created
    <<< stringify
    <<< encodeJson
