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

import Contract.Address (Address, NetworkId(TestnetId), addressWithNetworkTagToBech32)
import Contract.Hashing (datumHash)
import Contract.PlutusData (PlutusData(Constr, Map, List, Integer, Bytes), toData)
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex, byteLength, hexToByteArrayUnsafe)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionInput(TransactionInput), outputDatumDatum)
import Contract.Value (Value, getCurrencySymbol, getTokenName, getValue, valueToCoin')
import Ctl.Internal.Plutus.Conversion (toPlutusAddress)
import Ctl.Internal.Serialization (toBytes)
import Ctl.Internal.Serialization.Address (addressFromBech32)
import Ctl.Internal.Serialization.PlutusData (convertPlutusData)
import Ctl.Internal.Types.BigNum (toBigInt) as BigNum
import Data.Argonaut (class EncodeJson, Json, encodeJson, fromObject)
import Data.Array ((:))
import Data.Bifunctor (bimap)
import Data.BigInt (toNumber) as BigInt
import Data.Codec.Argonaut (JsonCodec, encode, json, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Int (hexadecimal, toStringAs)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (toString) as UInt
import Foreign.Object (delete, fromFoldable) as Obj
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

printOref :: TransactionInput -> String
printOref (TransactionInput rec) =
  byteArrayToHex (unwrap rec.transactionId) <> "#" <> UInt.toString rec.index

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

--

bytesToCbor :: ByteArray -> ByteArray
bytesToCbor ba =
  hexToByteArrayUnsafe ("59" <> toStringAs hexadecimal (byteLength ba))
    <> ba

plutusDataToCbor :: PlutusData -> ByteArray
plutusDataToCbor = unwrap <<< toBytes <<< convertPlutusData

encodeValue :: Value -> Json
encodeValue value =
  fromObject $ Obj.delete mempty $
    Obj.fromFoldable (lovelace : nonAdaAssets)
  where
  lovelace :: String /\ Json
  lovelace = "lovelace" /\ encodeJson (BigInt.toNumber $ valueToCoin' value)

  nonAdaAssets :: Array (String /\ Json)
  nonAdaAssets =
    unwrap (getValue value) <#> \(cs /\ mp) ->
      byteArrayToHex (getCurrencySymbol cs) /\
        ( fromObject $ Obj.fromFoldable $
            unwrap mp <#> \(tn /\ quantity) ->
              byteArrayToHex (getTokenName tn) /\ encodeJson (BigInt.toNumber quantity)
        )

encodePlutusData :: PlutusData -> Json
encodePlutusData = case _ of
  Constr constr fields ->
    encodeJson
      { constructor: BigInt.toNumber $ BigNum.toBigInt constr
      , fields: encodePlutusData <$> fields
      }
  Map kvs ->
    encodeJson
      { map:
          kvs <#> \(k /\ v) ->
            { k: encodePlutusData k, v: encodePlutusData v }
      }
  List xs ->
    encodeJson
      { list: encodePlutusData <$> xs
      }
  Integer bi ->
    encodeJson
      { int: BigInt.toNumber bi
      }
  Bytes ba ->
    encodeJson
      { bytes: byteArrayToHex ba
      }

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

--

addressCodec :: CA.JsonCodec Address
addressCodec =
  CA.prismaticCodec "Address"
    (toPlutusAddress <=< addressFromBech32)
    (addressWithNetworkTagToBech32 <<< wrap <<< { address: _, networkId: TestnetId })
    CA.string
