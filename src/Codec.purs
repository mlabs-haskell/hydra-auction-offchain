module HydraAuctionOffchain.Codec
  ( class HasJson
  , bigIntCodec
  , byteArrayCodec
  , currencySymbolCodec
  , fromJs
  , jsonCodec
  , liftAff1
  , liftAff2
  , orefCodec
  , posixTimeCodec
  , pubKeyHashCodec
  , toJs
  , tokenNameCodec
  , transactionHashCodec
  , valueCodec
  ) where

import Prelude

import Contract.Address (PubKeyHash)
import Contract.Prim.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Contract.Time (POSIXTime(POSIXTime))
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
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
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes, ed25519KeyHashToBytes)
import Data.Argonaut (Json)
import Data.BigInt (BigInt)
import Data.BigInt (fromString, toString) as BigInt
import Data.Codec.Argonaut
  ( JsonCodec
  , array
  , boolean
  , decode
  , encode
  , int
  , json
  , number
  , object
  , prismaticCodec
  , string
  ) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (fromRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap, wrapIso)
import Data.Tuple.Nested ((/\))
import Data.UInt (fromString, toString) as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(Proxy))
import Undefined (undefined)

currencySymbolCodec :: CA.JsonCodec CurrencySymbol
currencySymbolCodec =
  CA.prismaticCodec "CurrencySymbol" mkCurrencySymbol getCurrencySymbol byteArrayCodec

bigIntCodec :: CA.JsonCodec BigInt
bigIntCodec = CA.prismaticCodec "BigInt" BigInt.fromString BigInt.toString CA.string

byteArrayCodec :: CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex CA.string

orefCodec :: CA.JsonCodec TransactionInput
orefCodec =
  wrapIso TransactionInput $ CA.object "TransactionInput" $ CAR.record
    { transactionId: transactionHashCodec
    , index: CA.prismaticCodec "UInt" UInt.fromString UInt.toString CA.string
    }

posixTimeCodec :: CA.JsonCodec POSIXTime
posixTimeCodec = wrapIso POSIXTime bigIntCodec

pubKeyHashCodec :: CA.JsonCodec PubKeyHash
pubKeyHashCodec =
  CA.prismaticCodec "PubKeyHash" (map wrap <<< ed25519KeyHashFromBytes)
    (unwrap <<< ed25519KeyHashToBytes <<< unwrap)
    byteArrayCodec

tokenNameCodec :: CA.JsonCodec TokenName
tokenNameCodec =
  CA.prismaticCodec "TokenName" mkTokenName getTokenName byteArrayCodec

transactionHashCodec :: CA.JsonCodec TransactionHash
transactionHashCodec = wrapIso TransactionHash byteArrayCodec

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

--------------------------------------------------------------------------------
-- HasJson
--------------------------------------------------------------------------------

class HasJson a where
  jsonCodec :: Proxy a -> CA.JsonCodec a

toJs :: forall (a :: Type). HasJson a => a -> Json
toJs = CA.encode (jsonCodec (Proxy :: Proxy a))

fromJs :: forall (a :: Type). HasJson a => Json -> a
fromJs = fromRight undefined <<< CA.decode (jsonCodec (Proxy :: Proxy a))

liftAff1
  :: forall (a :: Type) (b :: Type)
   . HasJson a
  => HasJson b
  => (a -> Aff b)
  -> Json
  -> Effect (Promise Json)
liftAff1 f a = fromAff $ toJs <$> f (fromJs a)

liftAff2
  :: forall (a :: Type) (b :: Type) (c :: Type)
   . HasJson a
  => HasJson b
  => HasJson c
  => (a -> b -> Aff c)
  -> Json
  -> Json
  -> Effect (Promise Json)
liftAff2 f a b = fromAff $ toJs <$> f (fromJs a) (fromJs b)

instance HasJson Json where
  jsonCodec = const CA.json

instance HasJson Boolean where
  jsonCodec = const CA.boolean

instance HasJson String where
  jsonCodec = const CA.string

instance HasJson Number where
  jsonCodec = const CA.number

instance HasJson Int where
  jsonCodec = const CA.int

instance HasJson a => HasJson (Array a) where
  jsonCodec = const $ CA.array $ jsonCodec (Proxy :: Proxy a)

instance HasJson a => HasJson (Maybe a) where
  jsonCodec = const $ CA.maybe $ jsonCodec (Proxy :: Proxy a)

instance HasJson BigInt where
  jsonCodec = const bigIntCodec

instance HasJson ByteArray where
  jsonCodec = const byteArrayCodec

instance HasJson TransactionHash where
  jsonCodec = const transactionHashCodec
