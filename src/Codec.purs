module HydraAuctionOffchain.Codec
  ( class HasJson
  , bigIntCodec
  , byteArrayCodec
  , fromJs
  , jsonCodec
  , liftAff1
  , liftAff2
  , toJs
  , transactionHashCodec
  ) where

import Prelude

import Contract.Prim.ByteArray (ByteArray, byteArrayToHex, hexToByteArray)
import Contract.Transaction (TransactionHash(TransactionHash))
import Control.Promise (Promise, fromAff)
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
  , prismaticCodec
  , string
  ) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Either (fromRight)
import Data.Maybe (Maybe)
import Data.Profunctor (wrapIso)
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(Proxy))

bigIntCodec :: CA.JsonCodec BigInt
bigIntCodec = CA.prismaticCodec "BigInt" BigInt.fromString BigInt.toString CA.string

byteArrayCodec :: CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex CA.string

transactionHashCodec :: CA.JsonCodec TransactionHash
transactionHashCodec = wrapIso TransactionHash byteArrayCodec

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
