module HydraAuctionOffchain.Lib.Codec
  ( class HasJson
  , fromJs
  , jsonCodec
  , toJs
  ) where

import Prelude

import Aeson (Aeson, Finite)
import Cardano.Types (AssetName, BigNum, PublicKey)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionHash)
import Data.Codec.Argonaut
  ( JsonCodec
  , array
  , boolean
  , decode
  , encode
  , int
  , json
  , number
  , string
  ) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Either (hush)
import Data.Maybe (Maybe)
import HydraAuctionOffchain.Codec
  ( assetNameCodec
  , bigIntCodec
  , bigNumCodec
  , byteArrayCodec
  , publicKeyCodec
  , txHashCodec
  )
import HydraAuctionOffchain.Helpers (fromJustWithErr)
import JS.BigInt (BigInt)
import Type.Proxy (Proxy(Proxy))

class HasJson a params | a -> params where
  jsonCodec :: params -> Proxy a -> CA.JsonCodec a

toJs :: forall a p. HasJson a p => p -> a -> Aeson
toJs params = CA.encode (jsonCodec params (Proxy :: _ a))

fromJs :: forall a p. HasJson a p => p -> Aeson -> a
fromJs params = fromJustWithErr "fromJs" <<< hush <<< CA.decode (jsonCodec params Proxy)

instance HasJson Aeson anyParams where
  jsonCodec _ = const CA.json

instance HasJson Boolean anyParams where
  jsonCodec _ = const CA.boolean

instance HasJson String anyParams where
  jsonCodec _ = const CA.string

instance HasJson (Finite Number) anyParams where
  jsonCodec _ = const CA.number

instance HasJson Int anyParams where
  jsonCodec _ = const CA.int

instance HasJson a p => HasJson (Array a) p where
  jsonCodec params = const $ CA.array $ jsonCodec params Proxy

instance HasJson a p => HasJson (Maybe a) p where
  jsonCodec params = const $ CA.maybe $ jsonCodec params Proxy

instance HasJson BigInt anyParams where
  jsonCodec _ = const bigIntCodec

instance HasJson BigNum anyParams where
  jsonCodec _ = const bigNumCodec

instance HasJson ByteArray anyParams where
  jsonCodec _ = const byteArrayCodec

instance HasJson TransactionHash anyParams where
  jsonCodec _ = const txHashCodec

instance HasJson AssetName anyParams where
  jsonCodec _ = const assetNameCodec

instance HasJson PublicKey anyParams where
  jsonCodec _ = const publicKeyCodec
