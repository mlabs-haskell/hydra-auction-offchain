module HydraAuctionOffchain.Contract.DiscoverBidders
  ( BidderInfoCandidate(BidderInfoCandidate)
  , bidderInfoCandidateCodec
  , discoverBidders
  ) where

import Contract.Prelude hiding (oneOf)

import Contract.Monad (Contract)
import Data.Array.NonEmpty (fromArray)
import Data.BigInt (BigInt, fromInt, toInt)
import Data.Codec.Argonaut (JsonCodec, boolean, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (class HasJson, bigIntCodec)
import HydraAuctionOffchain.Contract.Types (AuctionInfo, BidderInfo, bidderInfoCodec)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, oneOf, randomSampleOne, vectorOf)

newtype BidderInfoCandidate = BidderInfoCandidate
  { bidderInfo :: BidderInfo
  , depositAmount :: BigInt
  , isValid :: Boolean
  }

derive instance Generic BidderInfoCandidate _
derive instance Newtype BidderInfoCandidate _
derive instance Eq BidderInfoCandidate

instance Show BidderInfoCandidate where
  show = genericShow

instance HasJson BidderInfoCandidate where
  jsonCodec = const bidderInfoCandidateCodec

bidderInfoCandidateCodec :: CA.JsonCodec BidderInfoCandidate
bidderInfoCandidateCodec =
  wrapIso BidderInfoCandidate $ CA.object "BidderInfoCandidate" $
    CAR.record
      { bidderInfo: bidderInfoCodec
      , depositAmount: bigIntCodec
      , isValid: CA.boolean
      }

genBidderInfoCandidate :: BigInt -> Gen BidderInfoCandidate
genBidderInfoCandidate minDepositAmount = do
  bidderInfo <- arbitrary
  let
    minAdaApprox = 1_500_000
    minDepositAmount' = max minDepositAmount $ fromInt $ minAdaApprox + one
  depositAmount <- oneOf $ unsafePartial fromJust $ fromArray
    [ add minDepositAmount' <<< fromInt <$> chooseInt zero 5_000_000
    , fromInt <$> chooseInt minAdaApprox (fromMaybe top $ toInt $ minDepositAmount' - one)
    ]
  pure $ wrap
    { bidderInfo
    , depositAmount
    , isValid: depositAmount >= minDepositAmount
    }

discoverBidders :: AuctionInfo -> Contract (Array BidderInfoCandidate)
discoverBidders = discoverBiddersStub

discoverBiddersStub :: AuctionInfo -> Contract (Array BidderInfoCandidate)
discoverBiddersStub auctionInfo =
  liftEffect $ randomSampleOne do
    numBidders <- chooseInt 2 10
    vectorOf numBidders $ genBidderInfoCandidate minDepositAmount
  where
  minDepositAmount :: BigInt
  minDepositAmount =
    (unwrap (unwrap auctionInfo).auctionTerms).minDepositAmount
