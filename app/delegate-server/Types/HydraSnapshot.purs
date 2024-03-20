module DelegateServer.Types.HydraSnapshot
  ( HydraSnapshot
  , emptySnapshot
  , hydraSnapshotCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, int, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import DelegateServer.Types.HydraUtxoMap (HydraUtxoMap, hydraUtxoMapCodec)

type HydraSnapshot =
  { snapshotNumber :: Int
  , utxo :: HydraUtxoMap
  }

emptySnapshot :: HydraSnapshot
emptySnapshot =
  { snapshotNumber: zero
  , utxo: mempty
  }

hydraSnapshotCodec :: CA.JsonCodec HydraSnapshot
hydraSnapshotCodec =
  CA.object "HydraSnapshot" $ CAR.record
    { snapshotNumber: CA.int
    , utxo: hydraUtxoMapCodec
    }
