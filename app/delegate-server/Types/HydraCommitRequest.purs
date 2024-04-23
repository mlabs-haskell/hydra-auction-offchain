module DelegateServer.Types.HydraCommitRequest
  ( HydraCommitRequest(SimpleCommitRequest, FullCommitRequest)
  , HydraBlueprintTx
  , HydraFullCommitRequest
  , hydraFullCommitRequestCodec
  , mkFullCommitRequest
  , mkSimpleCommitRequest
  ) where

import Prelude

import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (Transaction, TransactionHash)
import Contract.Utxos (UtxoMap)
import Ctl.Internal.Hashing (transactionHash)
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.Argonaut (class EncodeJson)
import Data.Codec.Argonaut (JsonCodec, encode, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import DelegateServer.Types.HydraUtxoMap (HydraUtxoMap, hydraUtxoMapCodec)
import DelegateServer.Types.HydraUtxoMap (fromUtxoMap) as HydraUtxoMap
import Effect (Effect)
import HydraAuctionOffchain.Codec (byteArrayCodec, transactionHashCodec)

data HydraCommitRequest
  = SimpleCommitRequest HydraUtxoMap
  | FullCommitRequest HydraFullCommitRequest

derive instance Generic HydraCommitRequest _
derive instance Eq HydraCommitRequest

instance Show HydraCommitRequest where
  show = genericShow

instance EncodeJson HydraCommitRequest where
  encodeJson =
    case _ of
      SimpleCommitRequest utxos ->
        CA.encode hydraUtxoMapCodec utxos
      FullCommitRequest rec ->
        CA.encode hydraFullCommitRequestCodec rec

mkSimpleCommitRequest :: UtxoMap -> HydraCommitRequest
mkSimpleCommitRequest = SimpleCommitRequest <<< HydraUtxoMap.fromUtxoMap

mkFullCommitRequest :: Transaction -> UtxoMap -> Effect HydraCommitRequest
mkFullCommitRequest tx utxos =
  convertTransaction tx <#> \cslTx ->
    FullCommitRequest
      { blueprintTx:
          { cborHex: unwrap $ toBytes cslTx
          , description: ""
          , txId: transactionHash cslTx
          , "type": "Tx BabbageEra"
          }
      , utxo: HydraUtxoMap.fromUtxoMap utxos
      }

type HydraFullCommitRequest =
  { blueprintTx :: HydraBlueprintTx
  , utxo :: HydraUtxoMap
  }

hydraFullCommitRequestCodec :: CA.JsonCodec HydraFullCommitRequest
hydraFullCommitRequestCodec =
  CA.object "HydraFullCommitRequest" $ CAR.record
    { blueprintTx: hydraBlueprintTxCodec
    , utxo: hydraUtxoMapCodec
    }

type HydraBlueprintTx =
  { cborHex :: ByteArray
  , description :: String
  , txId :: TransactionHash
  , "type" :: String
  }

hydraBlueprintTxCodec :: CA.JsonCodec HydraBlueprintTx
hydraBlueprintTxCodec =
  CA.object "HydraBlueprintTx" $ CAR.record
    { cborHex: byteArrayCodec
    , description: CA.string
    , txId: transactionHashCodec
    , "type": CA.string
    }
