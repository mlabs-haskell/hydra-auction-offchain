module HydraAuctionOffchain.Contract.Validators.Common
  ( reifySimpleValidator
  , reifyValidator
  ) where

import Prelude

import Aeson (decodeJsonString) as Aeson
import Contract.Scripts (Validator)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import HydraAuctionOffchain.Helpers (liftEitherShow)
import Ply.Reify (class ReifyParams)
import Ply.Reify (reifyTypedScript) as Ply
import Ply.TypeList (TyList)
import Ply.Types (TypedScript, ValidatorRole)
import Ply.Types (toValidator) as Ply

reifySimpleValidator :: forall m. MonadThrow Error m => String -> m Validator
reifySimpleValidator = map Ply.toValidator <<< reifyValidator

reifyValidator
  :: forall (m :: Type -> Type) (params :: TyList Type)
   . MonadThrow Error m
  => ReifyParams params
  => String
  -> m (TypedScript ValidatorRole params)
reifyValidator jsonStr = do
  raw <- liftEitherShow $ Aeson.decodeJsonString jsonStr
  liftEitherShow $ Ply.reifyTypedScript raw
