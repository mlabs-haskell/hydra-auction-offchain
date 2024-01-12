module HydraAuctionOffchain.Contract.QueryStandingBidState
  ( queryStandingBidState
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Control.Monad.Gen.Common (genMaybe)
import Data.BigInt (BigInt, fromInt)
import HydraAuctionOffchain.Contract.DiscoverSellerSignature (genSignature)
import HydraAuctionOffchain.Contract.Types (AuctionInfo, BidTerms, StandingBidState)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSampleOne)

queryStandingBidState :: AuctionInfo -> Contract StandingBidState
queryStandingBidState = queryStandingBidStateStub

queryStandingBidStateStub :: AuctionInfo -> Contract StandingBidState
queryStandingBidStateStub auctionInfo = do
  liftEffect $ randomSampleOne $ wrap <$> genMaybe (genBidTerms startingBid)
  where
  startingBid :: BigInt
  startingBid = (unwrap (unwrap auctionInfo).auctionTerms).startingBid

genBidTerms :: BigInt -> Gen BidTerms
genBidTerms startingBid = do
  bidder <- arbitrary
  bidDelta <- chooseInt zero 5_000_000
  let price = startingBid + fromInt bidDelta
  bidderSignature <- genSignature
  sellerSignature <- genSignature
  pure $ wrap { bidder, price, bidderSignature, sellerSignature }
