module DelegateServer.HydraNodeApi.Types.Commit
  ( CommitUtxoMap(CommitUtxoMap)
  , PlutusV2Script
  , PlutusV2ScriptType(PlutusScriptV2)
  , ScriptWitness
  , TxOutWithWitness
  , mkCollateralCommit
  , mkStandingBidCommit
  ) where

import Prelude

import Contract.Address (Address)
import Contract.Hashing (datumHash)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prim.ByteArray (ByteArray, byteLength, hexToByteArrayUnsafe)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionInput, outputDatumDatum)
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization.PlutusData (convertPlutusData)
import Data.Argonaut (class EncodeJson, Json, fromObject)
import Data.Bifunctor (bimap)
import Data.Codec.Argonaut (JsonCodec, encode, json, object, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import DelegateServer.Helpers (printOref)
import DelegateServer.Types.HydraUtxoMap (addressCodec, encodePlutusData, encodeValue)
import Foreign.Object (fromFoldable) as Obj
import HydraAuctionOffchain.Codec (byteArrayCodec)
import HydraAuctionOffchain.Contract.Types (StandingBidRedeemer(MoveToHydraRedeemer), Utxo)

newtype CommitUtxoMap = CommitUtxoMap (Array (TransactionInput /\ TxOutWithWitness))

derive instance Generic CommitUtxoMap _
derive instance Newtype CommitUtxoMap _
derive newtype instance Semigroup CommitUtxoMap
derive newtype instance Monoid CommitUtxoMap

instance EncodeJson CommitUtxoMap where
  encodeJson =
    fromObject
      <<< Obj.fromFoldable
      <<< map (bimap printOref (CA.encode txOutWithWitnessCodec))
      <<< unwrap

commitUtxoMapSingleton :: TransactionInput -> TxOutWithWitness -> CommitUtxoMap
commitUtxoMapSingleton oref txOutWithWitness =
  wrap [ oref /\ txOutWithWitness ]

--

mkCollateralCommit :: Utxo -> CommitUtxoMap
mkCollateralCommit (oref /\ txOut) =
  let
    rec = unwrap $ (unwrap txOut).output
  in
    commitUtxoMapSingleton oref
      { address: rec.address
      , value: encodeValue rec.amount
      , referenceScript: Nothing
      , datumhash: Nothing
      , inlineDatum: Nothing
      , inlineDatumhash: Nothing
      , datum: Nothing
      , witness: Nothing
      }

mkStandingBidCommit :: Utxo -> Validator -> Maybe CommitUtxoMap
mkStandingBidCommit (oref /\ txOut) standingBidValidator = do
  let
    rec = unwrap $ (unwrap txOut).output
    validatorBytes = fst $ unwrap $ unwrap standingBidValidator
  datum <- outputDatumDatum rec.datum
  pure $ commitUtxoMapSingleton oref
    { address: rec.address
    , value: encodeValue rec.amount
    , referenceScript: Nothing
    , datumhash: Nothing
    , inlineDatum: Just $ encodePlutusData $ unwrap datum
    , inlineDatumhash: Just $ unwrap $ datumHash datum
    , datum: Nothing
    , witness:
        Just
          { plutusV2Script:
              { cborHex: bytesToCbor validatorBytes
              , description: "Standing bid validator"
              , "type": PlutusScriptV2
              }
          , datum: Nothing
          , redeemer: plutusDataToCbor $ toData MoveToHydraRedeemer
          }
    }

bytesToCbor :: ByteArray -> ByteArray
bytesToCbor ba =
  hexToByteArrayUnsafe ("59" <> toStringAs hexadecimal (byteLength ba))
    <> ba

plutusDataToCbor :: PlutusData -> ByteArray
plutusDataToCbor = unwrap <<< toBytes <<< convertPlutusData

--

type TxOutWithWitness =
  { address :: Address
  , value :: Json
  , referenceScript :: Maybe Json
  , datumhash :: Maybe ByteArray
  , inlineDatum :: Maybe Json
  , inlineDatumhash :: Maybe ByteArray
  , datum :: Maybe ByteArray
  , witness :: Maybe ScriptWitness
  }

txOutWithWitnessCodec :: CA.JsonCodec TxOutWithWitness
txOutWithWitnessCodec =
  CA.object "TxOutWithWitness" $ CAR.record
    { address: addressCodec
    , value: CA.json
    , referenceScript: CA.maybe CA.json
    , datumhash: CA.maybe byteArrayCodec
    , inlineDatum: CA.maybe CA.json
    , inlineDatumhash: CA.maybe byteArrayCodec
    , datum: CA.maybe byteArrayCodec
    , witness: CA.maybe scriptWitnessCodec
    }

--

type ScriptWitness =
  { plutusV2Script :: PlutusV2Script
  , datum :: Maybe ByteArray
  , redeemer :: ByteArray
  }

scriptWitnessCodec :: CA.JsonCodec ScriptWitness
scriptWitnessCodec =
  CA.object "ScriptWitness" $ CAR.record
    { plutusV2Script: plutusV2ScriptCodec
    , datum: CA.maybe byteArrayCodec
    , redeemer: byteArrayCodec
    }

--

type PlutusV2Script =
  { cborHex :: ByteArray
  , description :: String
  , "type" :: PlutusV2ScriptType
  }

plutusV2ScriptCodec :: CA.JsonCodec PlutusV2Script
plutusV2ScriptCodec =
  CA.object "PlutusV2Script" $ CAR.record
    { cborHex: byteArrayCodec
    , description: CA.string
    , "type": plutusV2ScriptTypeCodec
    }

--

data PlutusV2ScriptType = PlutusScriptV2

derive instance Generic PlutusV2ScriptType _

plutusV2ScriptTypeCodec :: CA.JsonCodec PlutusV2ScriptType
plutusV2ScriptTypeCodec = CAG.nullarySum "PlutusV2ScriptType"
