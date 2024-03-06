module DelegateServer.Types.HydraSnapshot
  ( HydraSnapshot
  , hydraSnapshotCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import DelegateServer.Types.HydraUtxoMap (HydraUtxoMap, hydraUtxoMapCodec)

type HydraSnapshot =
  { utxo :: HydraUtxoMap
  }

hydraSnapshotCodec :: CA.JsonCodec HydraSnapshot
hydraSnapshotCodec =
  CA.object "HydraSnapshot" $ CAR.record
    { utxo: hydraUtxoMapCodec
    }
