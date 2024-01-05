module HydraAuctionOffchain.Contract.Types.Plutus.StandingBidState
  ( StandingBidState(StandingBidState)
  , validateNewBid
  ) where

import Prelude

import Contract.PlutusData (class FromData, class ToData)
import Contract.Value (CurrencySymbol)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Plutus.BidTerms (BidTerms, validateBidTerms)

newtype StandingBidState = StandingBidState (Maybe BidTerms)

derive instance Generic StandingBidState _
derive instance Newtype StandingBidState _
derive instance Eq StandingBidState
derive newtype instance ToData StandingBidState
derive newtype instance FromData StandingBidState

instance Show StandingBidState where
  show = genericShow

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateNewBid
  :: CurrencySymbol
  -> AuctionTerms
  -> StandingBidState
  -> StandingBidState
  -> Boolean
validateNewBid auctionCs auctionTerms oldBidState newBidState =
  case unwrap newBidState of
    Nothing -> false
    Just newTerms ->
      validateBidTerms auctionCs auctionTerms newTerms
        && validateCompareBids auctionTerms oldBidState newTerms

validateCompareBids :: AuctionTerms -> StandingBidState -> BidTerms -> Boolean
validateCompareBids auctionTerms oldBidState newTerms =
  case unwrap oldBidState of
    Nothing ->
      validateStartingBid auctionTerms newTerms
    Just oldTerms ->
      validateBidIncrement auctionTerms oldTerms newTerms

validateStartingBid :: AuctionTerms -> BidTerms -> Boolean
validateStartingBid auctionTerms newTerms =
  (unwrap auctionTerms).startingBid <= (unwrap newTerms).price

validateBidIncrement :: AuctionTerms -> BidTerms -> BidTerms -> Boolean
validateBidIncrement auctionTerms oldTerms newTerms =
  (unwrap oldTerms).price + (unwrap auctionTerms).minBidIncrement <= (unwrap newTerms).price
