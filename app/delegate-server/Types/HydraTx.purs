module DelegateServer.Types.HydraTx
  ( HydraTx
  , hydraTxCodec
  , mkHydraTx
  ) where

import Prelude

import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (Transaction)
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Newtype (unwrap)
import Effect (Effect)
import HydraAuctionOffchain.Codec (byteArrayCodec)

type HydraTx =
  { cborHex :: ByteArray
  , description :: String
  , "type" :: String
  }

hydraTxCodec :: CA.JsonCodec HydraTx
hydraTxCodec =
  CA.object "HydraBlueprintTx" $ CAR.record
    { cborHex: byteArrayCodec
    , description: CA.string
    , "type": CA.string
    }

mkHydraTx :: Transaction -> Effect HydraTx
mkHydraTx tx =
  convertTransaction tx <#> \cslTx ->
    { cborHex: unwrap $ toBytes cslTx
    , description: ""
    , "type": "Tx BabbageEra"
    }
