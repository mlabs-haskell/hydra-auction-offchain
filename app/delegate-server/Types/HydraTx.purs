module DelegateServer.Types.HydraTx
  ( HydraTx
  , hydraTxCodec
  , mkHydraTx
  ) where

import Prelude

import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (Transaction, TransactionHash)
import Ctl.Internal.Hashing (transactionHash)
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Newtype (unwrap)
import Effect (Effect)
import HydraAuctionOffchain.Codec (byteArrayCodec, transactionHashCodec)

type HydraTx =
  { cborHex :: ByteArray
  , description :: String
  , txId :: TransactionHash
  , "type" :: String
  }

hydraTxCodec :: CA.JsonCodec HydraTx
hydraTxCodec =
  CA.object "HydraBlueprintTx" $ CAR.record
    { cborHex: byteArrayCodec
    , description: CA.string
    , txId: transactionHashCodec
    , "type": CA.string
    }

mkHydraTx :: Transaction -> Effect HydraTx
mkHydraTx tx =
  convertTransaction tx <#> \cslTx ->
    { cborHex: unwrap $ toBytes cslTx
    , description: ""
    , txId: transactionHash cslTx
    , "type": "Tx BabbageEra"
    }
