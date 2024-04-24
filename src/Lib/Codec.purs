module HydraAuctionOffchain.Lib.Codec
  ( class HasJson
  , fixTaggedSumCodec
  , fromJs
  , jsonCodec
  , toJs
  ) where

import Prelude

import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionHash)
import Contract.Value (TokenName)
import Data.Argonaut (Json)
import Data.Argonaut (caseJsonObject, fromObject) as A
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut
  ( Codec(Codec)
  , JsonCodec
  , JsonDecodeError
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
import Data.Either (Either, hush)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple)
import Foreign.Object (delete, fromHomogeneous, lookup, member, size, union) as Obj
import HydraAuctionOffchain.Codec
  ( bigIntCodec
  , byteArrayCodec
  , tokenNameCodec
  , transactionHashCodec
  )
import HydraAuctionOffchain.Helpers (fromJustWithErr)
import JS.BigInt (BigInt)
import Type.Proxy (Proxy(Proxy))

fixTaggedSumCodec :: forall (a :: Type). CA.JsonCodec a -> CA.JsonCodec a
fixTaggedSumCodec (CA.Codec dec enc) = CA.Codec decFixed encFixed
  where
  decFixed :: Json -> Either CA.JsonDecodeError a
  decFixed json =
    dec
      ( json # A.caseJsonObject json \obj ->
          case Obj.lookup "tag" obj, Obj.size obj > one of
            Just tag, true ->
              A.fromObject $
                Obj.fromHomogeneous
                  { tag
                  , value: A.fromObject (Obj.delete "tag" obj)
                  }
            _, _ -> json
      )

  encFixed :: a -> Tuple Json a
  encFixed =
    enc >>> lmap \json ->
      json # A.caseJsonObject json \obj ->
        case Obj.member "tag" obj, Obj.lookup "value" obj of
          true, Just valueJson ->
            valueJson # A.caseJsonObject json \valueObj ->
              A.fromObject $ Obj.union valueObj $ Obj.delete "value" obj
          _, _ -> json

----------------------------------------------------------------------
-- HasJson

class HasJson a params | a -> params where
  jsonCodec :: params -> Proxy a -> CA.JsonCodec a

toJs :: forall a p. HasJson a p => p -> a -> Json
toJs params = CA.encode (jsonCodec params (Proxy :: _ a))

fromJs :: forall a p. HasJson a p => p -> Json -> a
fromJs params = fromJustWithErr "fromJs" <<< hush <<< CA.decode (jsonCodec params Proxy)

instance HasJson Json anyParams where
  jsonCodec _ = const CA.json

instance HasJson Boolean anyParams where
  jsonCodec _ = const CA.boolean

instance HasJson String anyParams where
  jsonCodec _ = const CA.string

instance HasJson Number anyParams where
  jsonCodec _ = const CA.number

instance HasJson Int anyParams where
  jsonCodec _ = const CA.int

instance HasJson a p => HasJson (Array a) p where
  jsonCodec params = const $ CA.array $ jsonCodec params Proxy

instance HasJson a p => HasJson (Maybe a) p where
  jsonCodec params = const $ CA.maybe $ jsonCodec params Proxy

instance HasJson BigInt anyParams where
  jsonCodec _ = const bigIntCodec

instance HasJson ByteArray anyParams where
  jsonCodec _ = const byteArrayCodec

instance HasJson TransactionHash anyParams where
  jsonCodec _ = const transactionHashCodec

instance HasJson TokenName anyParams where
  jsonCodec _ = const tokenNameCodec
