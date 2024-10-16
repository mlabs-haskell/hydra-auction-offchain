module DelegateServer.Contract.StandingBid
  ( queryStandingBidL2
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\))
import DelegateServer.State (class AppOpen, readAppState)
import HydraAuctionOffchain.Contract.QueryUtxo (findStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types (StandingBidState, Utxo)
import HydraSdk.Types (toUtxoMap)
import Type.Proxy (Proxy(Proxy))

queryStandingBidL2 :: forall m. AppOpen m => m (Maybe (Utxo /\ StandingBidState))
queryStandingBidL2 = do
  auctionInfo <- unwrap <$> readAppState (Proxy :: _ "auctionInfo")
  utxos <- toUtxoMap <<< _.utxo <<< unwrap <$> readAppState (Proxy :: _ "snapshot")
  pure $ findStandingBidUtxo auctionInfo utxos
