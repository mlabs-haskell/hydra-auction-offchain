module HydraAuctionOffchain.Lib.Script
  ( reifyScript
  , reifySimpleValidator
  ) where

import Prelude

import Aeson (decodeJsonString) as Aeson
import Contract.Scripts (Validator)
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import HydraAuctionOffchain.Helpers (liftEitherShow)
import Ply.Reify (class ReifyParams, class ReifyRole)
import Ply.Reify (reifyTypedScript) as Ply
import Ply.TypeList (TyList)
import Ply.Types (ScriptRole, TypedScript)
import Ply.Types (toValidator) as Ply

reifySimpleValidator :: forall m. MonadThrow Error m => String -> m Validator
reifySimpleValidator = map Ply.toValidator <<< reifyScript

reifyScript
  :: forall (m :: Type -> Type) (role :: ScriptRole) (params :: TyList Type)
   . MonadThrow Error m
  => ReifyRole role
  => ReifyParams params
  => String
  -> m (TypedScript role params)
reifyScript jsonStr = do
  raw <- liftEitherShow $ Aeson.decodeJsonString jsonStr
  liftEitherShow $ Ply.reifyTypedScript raw
