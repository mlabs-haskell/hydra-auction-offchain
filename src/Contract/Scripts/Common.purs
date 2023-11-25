module HydraAuctionOffchain.Contract.Scripts.Common
  ( reifySimpleValidator
  ) where

import Prelude

import Aeson (decodeJsonString) as Aeson
import Contract.Scripts (Validator)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import HydraAuctionOffchain.Helpers (liftEitherShow)
import Ply.Reify (reifyTypedScript) as Ply
import Ply.Types (toValidator) as Ply

reifySimpleValidator :: forall m. MonadThrow Error m => String -> m Validator
reifySimpleValidator jsonStr = do
  raw <- liftEitherShow $ Aeson.decodeJsonString jsonStr
  liftEitherShow $ Ply.toValidator <$> Ply.reifyTypedScript raw
