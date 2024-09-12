module DelegateServer.AppMap
  ( AppMap
  , buildAppMap
  ) where

import Prelude

import Cardano.Types (ScriptHash)
import Control.Safely (foldM)
import Data.List (fromFoldable) as List
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import DelegateServer.App (AppState)
import Effect.Aff (Aff)
import Effect.Aff.AVar (read) as AVar

type AppMap a = Map ScriptHash (AppState /\ a)

buildAppMap :: forall a. Array (AppState /\ a) -> Aff (AppMap a)
buildAppMap apps =
  foldM
    ( \appMap val@(appState /\ _) -> do
        auctionCs <- _.auctionId <<< unwrap <$> AVar.read appState.auctionInfo
        pure $ Map.insert auctionCs val appMap
    )
    Map.empty
    (List.fromFoldable apps)
