module DelegateServer.Types.HydraUtxoMap
  ( HydraUtxoMap(HydraUtxoMap)
  , addressCodec
  , encodePlutusData
  , encodeValue
  , hydraUtxoMapCodec
  , toUtxoMapWithoutRefScripts
  ) where

import Prelude

import Contract.Address (Address, NetworkId(TestnetId), addressWithNetworkTagToBech32)
import Contract.Hashing (datumHash)
import Contract.PlutusData
  ( OutputDatum(NoOutputDatum, OutputDatum)
  , PlutusData(Constr, Map, List, Integer, Bytes)
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  , outputDatumDatum
  )
import Contract.Utxos (UtxoMap)
import Contract.Value
  ( Value
  , getCurrencySymbol
  , getTokenName
  , getValue
  , lovelaceValueOf
  , mkCurrencySymbol
  , mkTokenName
  , valueToCoin'
  )
import Contract.Value (singleton) as Value
import Control.Alt ((<|>))
import Control.Safely (foldM)
import Ctl.Internal.Plutus.Conversion (toPlutusAddress)
import Ctl.Internal.Serialization.Address (addressFromBech32)
import Ctl.Internal.Types.BigNum (fromBigInt, toBigInt) as BigNum
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , Json
  , JsonDecodeError(AtKey, TypeMismatch, UnexpectedValue)
  , decodeJson
  , encodeJson
  , fromNumber
  , fromObject
  , fromString
  , (.:)
  )
import Data.Array ((:))
import Data.Bifunctor (bimap, lmap)
import Data.Bitraversable (bitraverse)
import Data.Codec.Argonaut (JsonCodec, decode, encode, json, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (optional, record) as CAR
import Data.Either (Either, hush, note)
import Data.Generic.Rep (class Generic)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import DelegateServer.Helpers (printOref, readOref)
import Foreign.Object (delete, fromFoldable, toUnfoldable) as Obj
import HydraAuctionOffchain.Codec (byteArrayCodec)
import HydraAuctionOffchain.Helpers (withoutRefScript)
import HydraAuctionOffchain.Lib.Json (fromCaJsonDecodeError)
import JS.BigInt (BigInt)
import JS.BigInt (fromNumber, toNumber) as BigInt

newtype HydraUtxoMap = HydraUtxoMap (Array (TransactionInput /\ TransactionOutput))

derive instance Generic HydraUtxoMap _
derive instance Newtype HydraUtxoMap _
derive newtype instance Semigroup HydraUtxoMap
derive newtype instance Monoid HydraUtxoMap

instance Show HydraUtxoMap where
  show = genericShow

instance EncodeJson HydraUtxoMap where
  encodeJson =
    fromObject
      <<< Obj.fromFoldable
      <<< map (bimap printOref (CA.encode txOutCodec))
      <<< unwrap

instance DecodeJson HydraUtxoMap where
  decodeJson =
    (map Obj.toUnfoldable <<< decodeJson) >=>
      ( map wrap <<< traverse
          ( bitraverse (note (TypeMismatch "TransactionInput") <<< readOref)
              (lmap fromCaJsonDecodeError <<< CA.decode txOutCodec)
          )
      )

hydraUtxoMapCodec :: CA.JsonCodec HydraUtxoMap
hydraUtxoMapCodec =
  CA.prismaticCodec "HydraUtxoMap" (hush <<< decodeJson) encodeJson
    CA.json

toUtxoMapWithoutRefScripts :: HydraUtxoMap -> UtxoMap
toUtxoMapWithoutRefScripts =
  Map.fromFoldable
    <<< map (map withoutRefScript)
    <<< unwrap

--

txOutCodec :: CA.JsonCodec TransactionOutput
txOutCodec =
  CA.prismaticCodec "TransactionOutput" (Just <<< fromHydraTxOut) toHydraTxOut
    hydraTxOutCodec
  where
  fromHydraTxOut :: HydraTxOut -> TransactionOutput
  fromHydraTxOut rec = wrap
    { address: rec.address
    , amount: rec.value
    , datum: maybe NoOutputDatum (OutputDatum <<< wrap) rec.inlineDatum
    , referenceScript: Nothing
    }

  toHydraTxOut :: TransactionOutput -> HydraTxOut
  toHydraTxOut (TransactionOutput rec) =
    { address: rec.address
    , value: rec.amount
    , inlineDatum: unwrap <$> outputDatumDatum rec.datum
    , inlineDatumhash: unwrap <<< datumHash <$> outputDatumDatum rec.datum
    }

--

type HydraTxOut =
  { address :: Address
  , value :: Value
  , inlineDatum :: Maybe PlutusData
  , inlineDatumhash :: Maybe ByteArray
  }

hydraTxOutCodec :: CA.JsonCodec HydraTxOut
hydraTxOutCodec =
  CA.object "HydraTxOut" $ CAR.record
    { address: addressCodec
    , value: valueCodec
    , inlineDatum: CA.maybe plutusDataCodec
    , inlineDatumhash: CAR.optional byteArrayCodec
    }

--

addressCodec :: CA.JsonCodec Address
addressCodec =
  CA.prismaticCodec "Address"
    (toPlutusAddress <=< addressFromBech32)
    (addressWithNetworkTagToBech32 <<< wrap <<< { address: _, networkId: TestnetId })
    CA.string

--

valueCodec :: CA.JsonCodec Value
valueCodec =
  CA.prismaticCodec "Value" (hush <<< decodeValue) encodeValue
    CA.json

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

decodeValue :: Json -> Either JsonDecodeError Value
decodeValue json = do
  obj <- decodeJson json
  let
    lovelaceKey = "lovelace"

    decodeLovelace :: Either JsonDecodeError BigInt
    decodeLovelace = do
      lovelaceNum <- obj .: lovelaceKey
      BigInt.fromNumber lovelaceNum #
        note (AtKey lovelaceKey $ UnexpectedValue $ fromNumber lovelaceNum)

    decodeNonAdaAssets :: Either JsonDecodeError Value
    decodeNonAdaAssets =
      foldM
        ( \acc (csStr /\ tnList) -> do
            cs <- note (TypeMismatch "CurrencySymbol") $ mkCurrencySymbol =<< hexToByteArray
              csStr
            tnObj <- Obj.toUnfoldable <$> decodeJson tnList
            foldM
              ( \acc' (tnStr /\ quantityNum) -> do
                  tn <- note (TypeMismatch "TokenName") $ mkTokenName =<< hexToByteArray tnStr
                  quantity <- BigInt.fromNumber quantityNum #
                    note (AtKey tnStr $ UnexpectedValue $ fromNumber quantityNum)
                  pure $ acc' <> Value.singleton cs tn quantity
              )
              acc
              tnObj
        )
        mempty
        (Obj.toUnfoldable $ Obj.delete lovelaceKey obj)

  lovelace <- decodeLovelace
  nonAdaAssets <- decodeNonAdaAssets
  pure $ nonAdaAssets <> lovelaceValueOf lovelace

--

plutusDataCodec :: CA.JsonCodec PlutusData
plutusDataCodec =
  CA.prismaticCodec "PlutusData" (hush <<< decodePlutusData) encodePlutusData
    CA.json

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

decodePlutusData :: Json -> Either JsonDecodeError PlutusData
decodePlutusData json = do
  obj <- decodeJson json
  let
    decodeConstr = do
      let constrKey = "constructor"
      constrNum <- obj .: constrKey
      constr <- (BigNum.fromBigInt =<< BigInt.fromNumber constrNum) #
        note (AtKey constrKey $ UnexpectedValue $ fromNumber constrNum)
      fields <- obj .: "fields"
      Constr constr <$> traverse decodePlutusData fields

    decodeMap = do
      (map' :: Array _) <- obj .: "map"
      Map <$> for map' \entry -> do
        key <- decodePlutusData =<< entry .: "k"
        value <- decodePlutusData =<< entry .: "v"
        pure $ key /\ value

    decodeList = do
      list <- obj .: "list"
      List <$> traverse decodePlutusData list

    decodeInteger = do
      let key = "int"
      num <- obj .: key
      Integer <$> BigInt.fromNumber num #
        note (AtKey key $ UnexpectedValue $ fromNumber num)

    decodeBytes = do
      let key = "bytes"
      bytesHex <- obj .: key
      Bytes <$> hexToByteArray bytesHex #
        note (AtKey key $ UnexpectedValue $ fromString bytesHex)

  decodeConstr
    <|> decodeMap
    <|> decodeList
    <|> decodeInteger
    <|> decodeBytes
