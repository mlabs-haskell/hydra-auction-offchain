module HydraAuctionOffchain.Contract.AuthorizeBidders
  ( AuthorizeBiddersContractParams(AuthorizeBiddersContractParams)
  , authorizeBiddersContract
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionHash)
import Contract.Value (CurrencySymbol)
import Data.Codec.Argonaut (JsonCodec, array, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Profunctor (wrapIso)
import HydraAuctionOffchain.Codec (class HasJson, byteArrayCodec, currencySymbolCodec)
import HydraAuctionOffchain.Contract.Types (ContractOutput(ContractOutputResult))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

newtype AuthorizeBiddersContractParams = AuthorizeBiddersContractParams
  { auctionCs :: CurrencySymbol
  , biddersToAuthorize :: Array ByteArray
  }

derive instance Generic AuthorizeBiddersContractParams _
derive instance Newtype AuthorizeBiddersContractParams _
derive instance Eq AuthorizeBiddersContractParams

instance Show AuthorizeBiddersContractParams where
  show = genericShow

instance HasJson AuthorizeBiddersContractParams where
  jsonCodec = const authorizeBiddersContractParamsCodec

authorizeBiddersContractParamsCodec :: CA.JsonCodec AuthorizeBiddersContractParams
authorizeBiddersContractParamsCodec =
  wrapIso AuthorizeBiddersContractParams $ CA.object "AuthorizeBiddersContractParams" $
    CAR.record
      { auctionCs: currencySymbolCodec
      , biddersToAuthorize: CA.array byteArrayCodec
      }

authorizeBiddersContract
  :: AuthorizeBiddersContractParams
  -> Contract (ContractOutput TransactionHash)
authorizeBiddersContract = const authorizeBiddersContractStub

authorizeBiddersContractStub :: Contract (ContractOutput TransactionHash)
authorizeBiddersContractStub = do
  txHash <- liftEffect $ randomSampleOne arbitrary
  pure $ ContractOutputResult txHash
