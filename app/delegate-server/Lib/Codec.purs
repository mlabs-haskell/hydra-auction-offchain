module DelegateServer.Lib.Codec
  ( fixTaggedSumCodec
  ) where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (caseJsonObject, fromObject) as A
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (Codec(Codec), JsonCodec, JsonDecodeError) as CA
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple)
import Foreign.Object (delete, fromHomogeneous, lookup, member, size, union) as Obj

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
