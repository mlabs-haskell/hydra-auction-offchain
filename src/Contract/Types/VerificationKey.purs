module HydraAuctionOffchain.Contract.Types.VerificationKey
  ( VerificationKey
  , vkeyBytes
  , vkeyCodec
  , vkeyFromBytes
  , vkeyLength
  ) where

import Prelude

import Contract.PlutusData (class FromData, class ToData)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToHex
  , byteLength
  )
import Data.Codec.Argonaut (JsonCodec, prismaticCodec) as CA
import Data.Maybe (Maybe(Just, Nothing))
import HydraAuctionOffchain.Codec (byteArrayCodec)
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, vectorOf)

newtype VerificationKey = VerificationKey ByteArray

derive instance Eq VerificationKey
derive instance Ord VerificationKey
derive newtype instance ToData VerificationKey
derive newtype instance FromData VerificationKey

instance Show VerificationKey where
  show (VerificationKey vkey) = "(VerificationKey " <> byteArrayToHex vkey <> ")"

instance HasJson VerificationKey anyParams where
  jsonCodec _ = const vkeyCodec

instance Arbitrary VerificationKey where
  arbitrary =
    vectorOf vkeyLength (chooseInt 0 255)
      <#> VerificationKey <<< byteArrayFromIntArrayUnsafe

-- All ed25519 keys have a fixed size of 256 bits (32 bytes).
-- https://eprint.iacr.org/2011/368.pdf
vkeyLength :: Int
vkeyLength = 32

vkeyFromBytes :: ByteArray -> Maybe VerificationKey
vkeyFromBytes bytes
  | byteLength bytes == vkeyLength = Just $ VerificationKey bytes
  | otherwise = Nothing

vkeyBytes :: VerificationKey -> ByteArray
vkeyBytes (VerificationKey vkey) = vkey

vkeyCodec :: CA.JsonCodec VerificationKey
vkeyCodec = CA.prismaticCodec "VerificationKey" vkeyFromBytes (\(VerificationKey x) -> x)
  byteArrayCodec
