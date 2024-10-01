module DelegateServer.Types.HydraTx
  ( HydraTx
  , hydraTxCodec
  , mkHydraTx
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (Transaction)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Newtype (unwrap)
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

mkHydraTx :: Transaction -> HydraTx
mkHydraTx tx =
  { cborHex: unwrap $ encodeCbor tx
  , description: ""
  , "type": "Tx ConwayEra"
  }
