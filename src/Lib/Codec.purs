module HydraAuctionOffchain.Lib.Codec
  ( class FromVariantGeneric
  , class HasJson
  , class ToVariantGeneric
  , fixTaggedSumCodec
  , fromJs
  , fromVariantGeneric
  , jsonCodec
  , sumGenericCodec
  , toJs
  , toVariantGeneric
  ) where

import Prelude

import Cardano.Types (AssetName, BigNum)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionHash)
import Contract.Value (TokenName)
import Control.Alt ((<|>))
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
  , prismaticCodec
  , string
  ) as CA
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Either (Either, hush)
import Data.Generic.Rep
  ( Argument(Argument)
  , Constructor(Constructor)
  , NoArguments(NoArguments)
  , Sum(Inl, Inr)
  , from
  , to
  ) as Generic
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Data.Variant (inj, prj) as Variant
import Foreign.Object (delete, fromHomogeneous, lookup, member, size, union) as Obj
import HydraAuctionOffchain.Codec
  ( assetNameCodec
  , bigIntCodec
  , bigNumCodec
  , byteArrayCodec
  , txHashCodec
  )
import HydraAuctionOffchain.Helpers (fromJustWithErr)
import JS.BigInt (BigInt)
import Prim.Row (class Cons) as Row
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

sumGenericCodec
  :: forall a rep row
   . Generic a rep
  => ToVariantGeneric rep row
  => FromVariantGeneric row rep
  => String
  -> JsonCodec (Variant row)
  -> JsonCodec a
sumGenericCodec typeName variantCodec =
  CA.prismaticCodec typeName (map Generic.to <<< fromVariantGeneric)
    (toVariantGeneric <<< Generic.from)
    variantCodec

class FromVariantGeneric row rep where
  fromVariantGeneric :: Variant row -> Maybe rep

instance
  ( FromVariantGeneric unionRow aRep
  , FromVariantGeneric unionRow bRep
  ) =>
  FromVariantGeneric unionRow (Generic.Sum aRep bRep) where
  fromVariantGeneric variant =
    (Generic.Inl <$> fromVariantGeneric variant)
      <|> (Generic.Inr <$> fromVariantGeneric variant)

else instance
  ( Row.Cons constrName Unit rowRest rowFull
  , IsSymbol constrName
  ) =>
  FromVariantGeneric rowFull (Generic.Constructor constrName Generic.NoArguments) where
  fromVariantGeneric variant =
    Variant.prj (Proxy :: _ constrName) variant <#> \_ ->
      Generic.Constructor Generic.NoArguments

else instance
  ( Row.Cons constrName value rowRest rowFull
  , IsSymbol constrName
  ) =>
  FromVariantGeneric rowFull (Generic.Constructor constrName (Generic.Argument value)) where
  fromVariantGeneric variant =
    Variant.prj (Proxy :: _ constrName) variant <#>
      Generic.Constructor <<< Generic.Argument

class ToVariantGeneric rep row where
  toVariantGeneric :: rep -> Variant row

instance
  ( Row.Cons constrName value rowRest rowFull
  , IsSymbol constrName
  ) =>
  ToVariantGeneric (Generic.Constructor constrName (Generic.Argument value)) rowFull where
  toVariantGeneric (Generic.Constructor (Generic.Argument x)) =
    Variant.inj (Proxy :: _ constrName) x

instance
  ( Row.Cons constrName Unit rowRest rowFull
  , IsSymbol constrName
  ) =>
  ToVariantGeneric (Generic.Constructor constrName Generic.NoArguments) rowFull where
  toVariantGeneric _ =
    Variant.inj (Proxy :: _ constrName) unit

instance
  ( ToVariantGeneric aRep unionRow
  , ToVariantGeneric bRep unionRow
  ) =>
  ToVariantGeneric (Generic.Sum aRep bRep) unionRow where
  toVariantGeneric = case _ of
    Generic.Inl x -> toVariantGeneric x
    Generic.Inr x -> toVariantGeneric x

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

instance HasJson BigNum anyParams where
  jsonCodec _ = const bigNumCodec

instance HasJson ByteArray anyParams where
  jsonCodec _ = const byteArrayCodec

instance HasJson TransactionHash anyParams where
  jsonCodec _ = const txHashCodec

instance HasJson AssetName anyParams where
  jsonCodec _ = const assetNameCodec
