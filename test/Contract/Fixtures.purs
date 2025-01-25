module Test.Contract.Fixtures
  ( auctionLotTokenNameFixture
  , auctionTermsInputFixture
  , delegatePkhFixture
  , minBidIncrementFixture
  , startingBidFixture
  ) where

import Prelude

import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Types (AssetName, Ed25519KeyHash)
import Cardano.Types.BigNum (fromInt)
import Contract.Time (POSIXTime)
import Data.Time.Duration (Seconds(Seconds))
import HydraAuctionOffchain.Contract.Types (AuctionTermsInput)
import HydraAuctionOffchain.Helpers (mkPosixTimeUnsafe)
import Test.Helpers (mkPubKeyHashUnsafe, mkTokenNameAsciiUnsafe)

auctionLotTokenNameFixture :: AssetName
auctionLotTokenNameFixture = mkTokenNameAsciiUnsafe "MonaLisa"

delegatePkhFixture :: Ed25519KeyHash
delegatePkhFixture =
  mkPubKeyHashUnsafe
    "ac55de689702d745e77050ce83b77ff9619383bb802e40fb90aa3be4"

auctionTermsInputFixture :: Plutus.Value -> POSIXTime -> AuctionTermsInput
auctionTermsInputFixture auctionLot biddingStart =
  { auctionLot
  , delegates: [ delegatePkhFixture ]
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
