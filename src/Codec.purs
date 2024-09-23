module HydraAuctionOffchain.Codec
  ( addressCodec
  , assetNameCodec
  , bigIntCodec
  , bigIntCodecNum
  , bigNumCodec
  , byteArrayCodec
  , ed25519KeyHashCodec
  , ed25519SignatureCodec
  , logLevelCodec
  , orefCodec
  , plutusAddressCodec
  , plutusPubKeyHashCodec
  , plutusValueCodec
  , portCodec
  , posixTimeCodec
  , publicKeyCodec
  , scriptHashCodec
  , serverConfigCodec
  , sysStartCodec
  , txCodec
  , txHashCodec
  , uuidCodec
  , vkeyWitnessCodec
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Address (fromCardano, toCardano) as Plutus.Address
import Cardano.Plutus.Types.CurrencySymbol (CurrencySymbol) as Plutus
import Cardano.Plutus.Types.CurrencySymbol (mkCurrencySymbol, unCurrencySymbol) as Plutus.CurrencySymbol
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(PubKeyHash)) as Plutus
import Cardano.Plutus.Types.TokenName (TokenName(TokenName)) as Plutus
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Plutus.Types.Value (flattenValue, singleton) as Plutus.Value
import Cardano.Types
  ( Address
  , AssetName
  , BigNum
  , CborBytes(CborBytes)
  , Ed25519KeyHash
  , Ed25519Signature
  , NetworkId
  , PublicKey
  , RawBytes(RawBytes)
  , ScriptHash
  , Transaction
  , TransactionHash
  , TransactionInput(TransactionInput)
  , Vkeywitness
  )
import Cardano.Types.Address (fromBech32, toBech32) as Address
import Cardano.Types.AssetName (mkAssetName, unAssetName)
import Cardano.Types.BigNum (fromString, toString) as BigNum
import Cardano.Types.PublicKey (fromRawBytes, toRawBytes) as PublicKey
import Contract.Config (ServerConfig)
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Contract.Time (POSIXTime(POSIXTime), SystemStart)
import Data.Codec.Argonaut
  ( JsonCodec
  , array
  , boolean
  , int
  , number
  , object
  , prismaticCodec
  , string
  ) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (hush)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (formatDateTime, unformatDateTime)
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap, wrapIso)
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt)
import Data.UInt (fromInt', fromString, toInt, toString) as UInt
import Data.UUID (UUID, parseUUID)
import Data.UUID (toString) as UUID
import HydraAuctionOffchain.Helpers (fromJustWithErr)
import JS.BigInt (BigInt)
import JS.BigInt (fromNumber, fromString, toNumber, toString) as BigInt
import URI.Port (Port)
import URI.Port (fromInt, toInt) as Port

addressCodec :: CA.JsonCodec Address
addressCodec =
  CA.prismaticCodec "Address" Address.fromBech32 Address.toBech32
    CA.string

asCborCodec :: forall a. AsCbor a => String -> CA.JsonCodec a
asCborCodec name =
  CA.prismaticCodec name decodeCbor encodeCbor
    cborBytesCodec

assetNameCodec :: CA.JsonCodec AssetName
assetNameCodec =
  CA.prismaticCodec "AssetName" mkAssetName unAssetName
    byteArrayCodec

bigIntCodec :: CA.JsonCodec BigInt
bigIntCodec =
  CA.prismaticCodec "BigInt" BigInt.fromString BigInt.toString
    CA.string

bigIntCodecNum :: CA.JsonCodec BigInt
bigIntCodecNum =
  CA.prismaticCodec "BigInt" BigInt.fromNumber BigInt.toNumber
    CA.number

bigNumCodec :: CA.JsonCodec BigNum
bigNumCodec =
  CA.prismaticCodec "BigNum" BigNum.fromString BigNum.toString
    CA.string

byteArrayCodec :: CA.JsonCodec ByteArray
byteArrayCodec =
  CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex
    CA.string

cborBytesCodec :: CA.JsonCodec CborBytes
cborBytesCodec = wrapIso CborBytes byteArrayCodec

ed25519KeyHashCodec :: CA.JsonCodec Ed25519KeyHash
ed25519KeyHashCodec = asCborCodec "Ed25519KeyHash"

ed25519SignatureCodec :: CA.JsonCodec Ed25519Signature
ed25519SignatureCodec =
  asCborCodec "Ed25519Signature"

logLevelCodec :: CA.JsonCodec LogLevel
logLevelCodec =
  CA.prismaticCodec "LogLevel" readLogLevel printLogLevel
    CA.string
  where
  readLogLevel :: String -> Maybe LogLevel
  readLogLevel = case _ of
    "trace" -> Just Trace
    "debug" -> Just Debug
    "info" -> Just Info
    "warn" -> Just Warn
    "error" -> Just Error
    _ -> Nothing

  printLogLevel :: LogLevel -> String
  printLogLevel = case _ of
    Trace -> "trace"
    Debug -> "debug"
    Info -> "info"
    Warn -> "warn"
    Error -> "error"

orefCodec :: CA.JsonCodec TransactionInput
orefCodec =
  wrapIso TransactionInput $ CA.object "TransactionInput" $ CAR.record
    { transactionId: txHashCodec
    , index: CA.prismaticCodec "UInt" UInt.fromString UInt.toString CA.string
    }

plutusAddressCodec :: NetworkId -> CA.JsonCodec Plutus.Address
plutusAddressCodec network =
  CA.prismaticCodec "Plutus.Address"
    Plutus.Address.fromCardano
    (fromJustWithErr "plutusAddressCodec" <<< Plutus.Address.toCardano network) -- FIXME
    addressCodec

plutusCurrencySymbolCodec :: CA.JsonCodec Plutus.CurrencySymbol
plutusCurrencySymbolCodec =
  CA.prismaticCodec
    "Plutus.CurrencySymbol"
    Plutus.CurrencySymbol.mkCurrencySymbol
    Plutus.CurrencySymbol.unCurrencySymbol
    byteArrayCodec

plutusPubKeyHashCodec :: CA.JsonCodec Plutus.PubKeyHash
plutusPubKeyHashCodec = wrapIso Plutus.PubKeyHash ed25519KeyHashCodec

plutusTokenNameCodec :: CA.JsonCodec Plutus.TokenName
plutusTokenNameCodec = wrapIso Plutus.TokenName assetNameCodec

type ValueEntry =
  { cs :: Plutus.CurrencySymbol
  , tn :: Plutus.TokenName
  , quantity :: BigInt
  }

plutusValueCodec :: CA.JsonCodec Plutus.Value
plutusValueCodec = dimap fromValue toValue $ CA.array valueEntryCodec
  where
  fromValue :: Plutus.Value -> Array ValueEntry
  fromValue =
    map (\(cs /\ tn /\ quantity) -> { cs, tn, quantity })
      <<< Plutus.Value.flattenValue

  toValue :: Array ValueEntry -> Plutus.Value
  toValue = foldMap \rec -> Plutus.Value.singleton rec.cs rec.tn rec.quantity

  valueEntryCodec :: CA.JsonCodec ValueEntry
  valueEntryCodec = CA.object "ValueEntry" $ CAR.record
    { cs: plutusCurrencySymbolCodec
    , tn: plutusTokenNameCodec
    , quantity: bigIntCodec
    }

portCodec :: CA.JsonCodec Port
portCodec = CA.prismaticCodec "Port" Port.fromInt Port.toInt CA.int

posixTimeCodec :: CA.JsonCodec POSIXTime
posixTimeCodec = wrapIso POSIXTime bigIntCodec

publicKeyCodec :: CA.JsonCodec PublicKey
publicKeyCodec =
  CA.prismaticCodec "PublicKey" PublicKey.fromRawBytes PublicKey.toRawBytes
    rawBytesCodec

rawBytesCodec :: CA.JsonCodec RawBytes
rawBytesCodec = wrapIso RawBytes byteArrayCodec

scriptHashCodec :: CA.JsonCodec ScriptHash
scriptHashCodec = asCborCodec "ScriptHash"

serverConfigCodec :: CA.JsonCodec ServerConfig
serverConfigCodec =
  CA.object "ServerConfig" $ CAR.record
    { port: uintCodec
    , host: CA.string
    , secure: CA.boolean
    , path: CA.maybe CA.string
    }

sysStartCodec :: CA.JsonCodec SystemStart
sysStartCodec =
  CA.prismaticCodec "SystemStart"
    (map wrap <<< hush <<< unformatDateTime formatter)
    (fromJustWithErr "sysStartCodec" <<< hush <<< formatDateTime formatter <<< unwrap)
    CA.string
  where
  formatter :: String
  formatter = "YYYY-MM-DDTHH:mm:ssZ"

txCodec :: CA.JsonCodec Transaction
txCodec = asCborCodec "Transaction"

txHashCodec :: CA.JsonCodec TransactionHash
txHashCodec = asCborCodec "TransactionHash"

uintCodec :: CA.JsonCodec UInt
uintCodec = CA.prismaticCodec "UInt" UInt.fromInt' UInt.toInt CA.int

uuidCodec :: CA.JsonCodec UUID
uuidCodec =
  CA.prismaticCodec "UUID" parseUUID UUID.toString
    CA.string

vkeyWitnessCodec :: CA.JsonCodec Vkeywitness
vkeyWitnessCodec = asCborCodec "Vkeywitness"
