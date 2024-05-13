module DelegateServer.Lib.AVar
  ( modifyAVar
  , modifyAVar_
  ) where

import Prelude

import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (put, take) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)

modifyAVar :: forall m a. MonadAff m => AVar a -> (a -> m a) -> m a
modifyAVar avar f = do
  val <- f =<< liftAff (AVar.take avar)
  liftAff $ AVar.put val avar
  pure val

modifyAVar_ :: forall m a. MonadAff m => AVar a -> (a -> m a) -> m Unit
modifyAVar_ avar f = do
  val <- f =<< liftAff (AVar.take avar)
  liftAff $ AVar.put val avar
