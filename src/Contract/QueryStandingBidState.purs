module HydraAuctionOffchain.Contract.QueryStandingBidState
  ( queryStandingBidState
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Control.Monad.Gen.Common (genMaybe)
import Data.BigInt (BigInt, fromInt)
import HydraAuctionOffchain.Contract.Types (AuctionInfo, BidTerms, StandingBidState)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSampleOne, vectorOf)

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

genSignature :: Gen ByteArray
genSignature = byteArrayFromIntArrayUnsafe <$> vectorOf 20 (chooseInt 0 255)
