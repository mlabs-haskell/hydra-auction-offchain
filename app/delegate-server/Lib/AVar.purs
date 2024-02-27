module DelegateServer.Lib.AVar
  ( modifyAVar
  , modifyAVar_
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (put, take) as AVar

modifyAVar :: forall (a :: Type). AVar a -> (a -> Aff a) -> Aff a
modifyAVar avar f = do
  val <- f =<< AVar.take avar
  AVar.put val avar
  pure val

modifyAVar_ :: forall (a :: Type). AVar a -> (a -> Aff a) -> Aff Unit
modifyAVar_ avar f = do
  val <- f =<< AVar.take avar
  AVar.put val avar
