module HydraAuctionOffchain.Contract.DiscoverBidders
  ( discoverBiddersContract
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import HydraAuctionOffchain.Contract.Types (AuctionInfo, BidderInfo)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (chooseInt, randomSampleOne, vectorOf)

discoverBiddersContract :: AuctionInfo -> Contract (Array BidderInfo)
discoverBiddersContract = const discoverBiddersContractStub

discoverBiddersContractStub :: Contract (Array BidderInfo)
discoverBiddersContractStub =
  liftEffect $ randomSampleOne
    (chooseInt 2 10 >>= flip vectorOf arbitrary)
