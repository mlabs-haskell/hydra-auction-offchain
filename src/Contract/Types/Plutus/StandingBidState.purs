module HydraAuctionOffchain.Contract.Types.Plutus.StandingBidState
  ( StandingBidState(StandingBidState)
  , standingBidStateCodec
  , validateNewBid
  ) where

import Prelude

import Cardano.Types (NetworkId)
import Cardano.Types.BigNum (add) as BigNum
import Contract.PlutusData (class FromData, class ToData)
import Contract.Value (CurrencySymbol)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Plutus.BidTerms
  ( BidTerms
  , bidTermsCodec
  , validateBidTerms
  )
import HydraAuctionOffchain.Lib.Codec (class HasJson)

newtype StandingBidState = StandingBidState (Maybe BidTerms)

derive instance Generic StandingBidState _
derive instance Newtype StandingBidState _
derive instance Eq StandingBidState
derive newtype instance ToData StandingBidState
derive newtype instance FromData StandingBidState

instance Show StandingBidState where
  show = genericShow

instance HasJson StandingBidState NetworkId where
  jsonCodec network = const (standingBidStateCodec network)

standingBidStateCodec :: NetworkId -> CA.JsonCodec StandingBidState
standingBidStateCodec network =
  wrapIso StandingBidState $
    CA.maybe (bidTermsCodec network)

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateNewBid
  :: NetworkId
  -> CurrencySymbol
  -> AuctionTerms
  -> StandingBidState
  -> StandingBidState
  -> Effect Boolean
validateNewBid network auctionCs auctionTerms oldBidState newBidState =
  case unwrap newBidState of
    Nothing -> pure false
    Just newTerms ->
      conj (validateCompareBids auctionTerms oldBidState newTerms) <$>
        validateBidTerms network auctionCs auctionTerms newTerms

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
  maybe false (_ <= (unwrap newTerms).price) $
    BigNum.add (unwrap oldTerms).price (unwrap auctionTerms).minBidIncrement
