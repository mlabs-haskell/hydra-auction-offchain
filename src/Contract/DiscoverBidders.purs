module HydraAuctionOffchain.Contract.DiscoverBidders
  ( discoverBidders
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import HydraAuctionOffchain.Contract.Types (AuctionInfo, BidderInfo)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (chooseInt, randomSampleOne, vectorOf)

discoverBidders :: AuctionInfo -> Contract (Array BidderInfo)
discoverBidders = const discoverBiddersStub

discoverBiddersStub :: Contract (Array BidderInfo)
discoverBiddersStub =
  liftEffect $ randomSampleOne
    (chooseInt 2 10 >>= flip vectorOf arbitrary)
