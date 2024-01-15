module HydraAuctionOffchain.Contract.Types.Plutus.StandingBidState
  ( StandingBidState(StandingBidState)
  , standingBidStateCodec
  , validateNewBid
  ) where

import Prelude

import Contract.PlutusData (class FromData, class ToData)
import Contract.Value (CurrencySymbol)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import HydraAuctionOffchain.Codec (class HasJson)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Plutus.BidTerms
  ( BidTerms
  , bidTermsCodec
  , validateBidTerms
  )

newtype StandingBidState = StandingBidState (Maybe BidTerms)

derive instance Generic StandingBidState _
derive instance Newtype StandingBidState _
derive instance Eq StandingBidState
derive newtype instance ToData StandingBidState
derive newtype instance FromData StandingBidState

instance Show StandingBidState where
  show = genericShow

instance HasJson StandingBidState where
  jsonCodec = const standingBidStateCodec

standingBidStateCodec :: CA.JsonCodec StandingBidState
standingBidStateCodec = wrapIso StandingBidState $ CA.maybe bidTermsCodec

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateNewBid
  :: CurrencySymbol
  -> AuctionTerms
  -> StandingBidState
  -> StandingBidState
  -> Effect Boolean
validateNewBid auctionCs auctionTerms oldBidState newBidState =
  case unwrap newBidState of
    Nothing -> pure false
    Just newTerms ->
      conj (validateCompareBids auctionTerms oldBidState newTerms) <$>
        validateBidTerms auctionCs auctionTerms newTerms

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
