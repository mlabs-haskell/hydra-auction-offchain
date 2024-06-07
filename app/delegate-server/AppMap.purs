module DelegateServer.AppMap
  ( AppMap
  , buildAppMap
  ) where

import Prelude

import Contract.Value (CurrencySymbol)
import Control.Safely (foldM)
import Data.List (fromFoldable) as List
import Data.Map (Map)
import Data.Map (empty, insert) as Map
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import DelegateServer.App (AppState)
import Effect.Aff (Aff)
import Effect.Aff.AVar (read) as AVar

type AppMap a = Map CurrencySymbol (AppState /\ a)

buildAppMap :: Array AppState -> Aff (AppMap Unit)
buildAppMap apps =
  foldM
    ( \appMap appState -> do
        auctionCs <- _.auctionId <<< unwrap <$> AVar.read appState.auctionInfo
        pure $ Map.insert auctionCs (appState /\ unit) appMap
    )
    Map.empty
    (List.fromFoldable apps)
