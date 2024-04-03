module Test.Contract.Fixtures
  ( auctionLotTokenNameFixture
  , auctionTermsInputFixture
  , delegatePkhFixture
  , minBidIncrementFixture
  , startingBidFixture
  ) where

import Prelude

import Contract.Address (PubKeyHash)
import Contract.Time (POSIXTime)
import Contract.Value (TokenName, Value)
import Data.Time.Duration (Seconds(Seconds))
import HydraAuctionOffchain.Contract.Types (AuctionTermsInput)
import HydraAuctionOffchain.Helpers (mkPosixTimeUnsafe)
import JS.BigInt (fromInt)
import Test.Helpers (mkPubKeyHashUnsafe, mkTokenNameAsciiUnsafe)

auctionLotTokenNameFixture :: TokenName
auctionLotTokenNameFixture = mkTokenNameAsciiUnsafe "MonaLisa"

delegatePkhFixture :: PubKeyHash
delegatePkhFixture =
  mkPubKeyHashUnsafe
    "ac55de689702d745e77050ce83b77ff9619383bb802e40fb90aa3be4"

auctionTermsInputFixture :: Value -> POSIXTime -> Array PubKeyHash -> AuctionTermsInput
auctionTermsInputFixture auctionLot biddingStart delegates =
  { auctionLot
  , delegates
  , biddingStart
  , biddingEnd
  , purchaseDeadline
  , cleanup
  , auctionFeePerDelegate: fromInt 3_000_000
  , startingBid: fromInt startingBidFixture
  , minBidIncrement: fromInt minBidIncrementFixture
  , minDepositAmount: fromInt 3_000_000
  }
  where
  defaultPeriod = mkPosixTimeUnsafe (Seconds 300.0)
  biddingEnd = biddingStart + defaultPeriod
  purchaseDeadline = biddingEnd + defaultPeriod
  cleanup = purchaseDeadline + defaultPeriod

startingBidFixture :: Int
startingBidFixture = 20_000_000

minBidIncrementFixture :: Int
minBidIncrementFixture = 5_000_000
