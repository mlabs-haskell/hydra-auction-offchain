module HydraAuctionOffchain.Contract.EnterAuction
  ( EnterAuctionContractParams(EnterAuctionContractParams)
  , enterAuctionContract
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Transaction (TransactionHash)
import Data.BigInt (BigInt)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Profunctor (wrapIso)
import HydraAuctionOffchain.Codec (class HasJson, bigIntCodec)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfo
  , ContractOutput(ContractOutputResult)
  , auctionInfoCodec
  )
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

newtype EnterAuctionContractParams = EnterAuctionContractParams
  { auctionInfo :: AuctionInfo
  , depositAmount :: BigInt
  }

derive instance Generic EnterAuctionContractParams _
derive instance Newtype EnterAuctionContractParams _
derive instance Eq EnterAuctionContractParams

instance Show EnterAuctionContractParams where
  show = genericShow

instance HasJson EnterAuctionContractParams where
  jsonCodec = const enterAuctionContractParamsCodec

enterAuctionContractParamsCodec :: CA.JsonCodec EnterAuctionContractParams
enterAuctionContractParamsCodec =
  wrapIso EnterAuctionContractParams $ CA.object "EnterAuctionContractParams" $
    CAR.record
      { auctionInfo: auctionInfoCodec
      , depositAmount: bigIntCodec
      }

enterAuctionContract
  :: EnterAuctionContractParams
  -> Contract (ContractOutput TransactionHash)
enterAuctionContract = const enterAuctionContractStub

enterAuctionContractStub :: Contract (ContractOutput TransactionHash)
enterAuctionContractStub = do
  txHash <- liftEffect $ randomSampleOne arbitrary
  pure $ ContractOutputResult txHash
