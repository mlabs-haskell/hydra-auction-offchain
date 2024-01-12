module HydraAuctionOffchain.Contract.DiscoverBidders
  ( discoverBidders
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import HydraAuctionOffchain.Contract.Types (AuctionInfo, BidderInfo)
import Undefined (undefined)

discoverBidders :: AuctionInfo -> Contract (Array BidderInfo)
discoverBidders = const discoverBiddersStub

discoverBiddersStub :: Contract (Array BidderInfo)
discoverBiddersStub = undefined
