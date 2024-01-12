module HydraAuctionOffchain.Contract.DiscoverSellerSignature
  ( DiscoverSellerSigContractParams(DiscoverSellerSigContractParams)
  , discoverSellerSigContract
  ) where

import Contract.Prelude

import Contract.Address (PubKeyHash)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Contract.Value (CurrencySymbol)
import Control.Monad.Gen.Common (genMaybe)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (class HasJson, currencySymbolCodec, pubKeyHashCodec)
import Test.QuickCheck.Gen (chooseInt, randomSampleOne, vectorOf)

newtype DiscoverSellerSigContractParams = DiscoverSellerSigContractParams
  { auctionCs :: CurrencySymbol
  , sellerPkh :: PubKeyHash
  }

derive instance Generic DiscoverSellerSigContractParams _
derive instance Newtype DiscoverSellerSigContractParams _
derive instance Eq DiscoverSellerSigContractParams

instance Show DiscoverSellerSigContractParams where
  show = genericShow

instance HasJson DiscoverSellerSigContractParams where
  jsonCodec = const discoverSellerSigContractParamsCodec

discoverSellerSigContractParamsCodec :: CA.JsonCodec DiscoverSellerSigContractParams
discoverSellerSigContractParamsCodec =
  wrapIso DiscoverSellerSigContractParams $ CA.object "DiscoverSellerSigContractParams" $
    CAR.record
      { auctionCs: currencySymbolCodec
      , sellerPkh: pubKeyHashCodec
      }

discoverSellerSigContract :: DiscoverSellerSigContractParams -> Contract (Maybe ByteArray)
discoverSellerSigContract = const discoverSellerSigContractStub

discoverSellerSigContractStub :: Contract (Maybe ByteArray)
discoverSellerSigContractStub =
  liftEffect $ randomSampleOne
    (genMaybe $ byteArrayFromIntArrayUnsafe <$> vectorOf 20 (chooseInt 0 255))
