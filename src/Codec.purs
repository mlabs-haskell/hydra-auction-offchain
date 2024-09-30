module HydraAuctionOffchain.Codec
  ( addressCodec
  , addressWithNetworkTagCodec
  , bigIntCodec
  , bigIntCodecNum
  , byteArrayCodec
  , currencySymbolCodec
  , ed25519SignatureCodec
  , logLevelCodec
  , orefCodec
  , portCodec
  , posixTimeCodec
  , pubKeyHashCodec
  , publicKeyCodec
  , serverConfigCodec
  , sysStartCodec
  , tokenNameCodec
  , transactionHashCodec
  , txCodec
  , valueCodec
  , vkeyWitnessCodec
  ) where

import Prelude

import Contract.Address
  ( Address
  , AddressWithNetworkTag
  , PubKeyHash
  , addressWithNetworkTagFromBech32
  , addressWithNetworkTagToBech32
  )
import Contract.Config (NetworkId, ServerConfig)
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Contract.Time (POSIXTime(POSIXTime), SystemStart)
import Contract.Transaction
  ( Ed25519Signature
  , PublicKey
  , Transaction
  , TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  )
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , getCurrencySymbol
  , getTokenName
  , mkCurrencySymbol
  , mkTokenName
  )
import Contract.Value (flattenValue, singleton) as Value
import Ctl.Internal.Cardano.Types.Transaction
  ( convertEd25519Signature
  , convertPubKey
  , mkFromCslEd25519Signature
  , mkFromCslPubKey
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.Transaction (deserializeTransaction)
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes, ed25519KeyHashToBytes)
import Ctl.Internal.Serialization.Keys (bytesFromPublicKey)
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
import Data.Codec.Argonaut.Compat (maybe, tuple) as CA
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
import Effect.Unsafe (unsafePerformEffect)
import HydraAuctionOffchain.Helpers (fromJustWithErr)
import JS.BigInt (BigInt)
import JS.BigInt (fromNumber, fromString, toNumber, toString) as BigInt
import URI.Port (Port)
import URI.Port (fromInt, toInt) as Port

addressCodec :: NetworkId -> CA.JsonCodec Address
addressCodec network =
  dimap (wrap <<< { address: _, networkId: network }) (_.address <<< unwrap)
    addressWithNetworkTagCodec

addressWithNetworkTagCodec :: CA.JsonCodec AddressWithNetworkTag
addressWithNetworkTagCodec =
  CA.prismaticCodec "AddressWithNetworkTag" addressWithNetworkTagFromBech32
    addressWithNetworkTagToBech32
    CA.string

currencySymbolCodec :: CA.JsonCodec CurrencySymbol
currencySymbolCodec =
  CA.prismaticCodec "CurrencySymbol" mkCurrencySymbol getCurrencySymbol byteArrayCodec

bigIntCodec :: CA.JsonCodec BigInt
bigIntCodec = CA.prismaticCodec "BigInt" BigInt.fromString BigInt.toString CA.string

bigIntCodecNum :: CA.JsonCodec BigInt
bigIntCodecNum = CA.prismaticCodec "BigInt" BigInt.fromNumber BigInt.toNumber CA.number

byteArrayCodec :: CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex CA.string

logLevelCodec :: CA.JsonCodec LogLevel
logLevelCodec = CA.prismaticCodec "LogLevel" readLogLevel printLogLevel CA.string
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
    { transactionId: transactionHashCodec
    , index: CA.prismaticCodec "UInt" UInt.fromString UInt.toString CA.string
    }

portCodec :: CA.JsonCodec Port
portCodec = CA.prismaticCodec "Port" Port.fromInt Port.toInt CA.int

posixTimeCodec :: CA.JsonCodec POSIXTime
posixTimeCodec = wrapIso POSIXTime bigIntCodec

pubKeyHashCodec :: CA.JsonCodec PubKeyHash
pubKeyHashCodec =
  CA.prismaticCodec "PubKeyHash" (map wrap <<< ed25519KeyHashFromBytes)
    (unwrap <<< ed25519KeyHashToBytes <<< unwrap)
    byteArrayCodec

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

tokenNameCodec :: CA.JsonCodec TokenName
tokenNameCodec =
  CA.prismaticCodec "TokenName" mkTokenName getTokenName byteArrayCodec

transactionHashCodec :: CA.JsonCodec TransactionHash
transactionHashCodec = wrapIso TransactionHash byteArrayCodec

txCodec :: CA.JsonCodec Transaction
txCodec =
  CA.prismaticCodec
    "Transaction"
    (hush <<< deserializeTransaction <<< wrap)
    (unwrap <<< toBytes <<< unsafePerformEffect <<< convertTransaction)
    byteArrayCodec

uintCodec :: CA.JsonCodec UInt
uintCodec = CA.prismaticCodec "UInt" UInt.fromInt' UInt.toInt CA.int

valueCodec :: CA.JsonCodec Value
valueCodec = dimap fromValue toValue $ CA.array valueEntryCodec
  where
  fromValue :: Value -> Array ValueEntry
  fromValue = map (\(cs /\ tn /\ quantity) -> { cs, tn, quantity }) <<< Value.flattenValue

  toValue :: Array ValueEntry -> Value
  toValue = foldMap \rec -> Value.singleton rec.cs rec.tn rec.quantity

type ValueEntry =
  { cs :: CurrencySymbol
  , tn :: TokenName
  , quantity :: BigInt
  }

valueEntryCodec :: CA.JsonCodec ValueEntry
valueEntryCodec = CA.object "ValueEntry" $ CAR.record
  { cs: currencySymbolCodec
  , tn: tokenNameCodec
  , quantity: bigIntCodec
  }

vkeyWitnessCodec :: CA.JsonCodec Vkeywitness
vkeyWitnessCodec =
  wrapIso Vkeywitness $
    CA.tuple (wrapIso Vkey publicKeyCodec) ed25519SignatureCodec

publicKeyCodec :: CA.JsonCodec PublicKey
publicKeyCodec =
  CA.prismaticCodec "PublicKey"
    (map mkFromCslPubKey <<< fromBytes <<< wrap)
    (unwrap <<< bytesFromPublicKey <<< convertPubKey)
    byteArrayCodec

ed25519SignatureCodec :: CA.JsonCodec Ed25519Signature
ed25519SignatureCodec =
  CA.prismaticCodec "Ed25519Signature"
    (map mkFromCslEd25519Signature <<< fromBytes <<< wrap)
    (unwrap <<< toBytes <<< convertEd25519Signature)
    byteArrayCodec
