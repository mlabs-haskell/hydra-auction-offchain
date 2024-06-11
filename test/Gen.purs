module Test.Gen
  ( genBidTerms
  , genBidderInfo
  , genPubKeyHashAddress
  , genStandingBid
  ) where

import Prelude

import Contract.Address (Address, pubKeyHashAddress)
import Control.Monad.Gen.Common (genMaybe)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
import HydraAuctionOffchain.Contract.Types (BidTerms, BidderInfo, StandingBidState)
import JS.BigInt (fromInt) as BigInt
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt)

genStandingBid :: Gen StandingBidState
genStandingBid = wrap <$> genMaybe genBidTerms

genBidTerms :: Gen BidTerms
genBidTerms = do
  bidder <- genBidderInfo
  price <- BigInt.fromInt <$> chooseInt zero top
  bidderSignature <- arbitrary
  sellerSignature <- arbitrary
  pure $ wrap
    { bidder
    , price
    , bidderSignature
    , sellerSignature
    }

genBidderInfo :: Gen BidderInfo
genBidderInfo = do
  bidderAddress <- genPubKeyHashAddress
  bidderVk <- arbitrary
  pure $ wrap
    { bidderAddress
    , bidderVk
    }

genPubKeyHashAddress :: Gen Address
genPubKeyHashAddress =
  arbitrary <#>
    flip pubKeyHashAddress Nothing <<< wrap <<< wrap
