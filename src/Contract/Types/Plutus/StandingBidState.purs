module HydraAuctionOffchain.Contract.Types.Plutus.StandingBidState
  ( StandingBidState(StandingBidState)
  ) where

import Prelude

import Contract.PlutusData (class FromData, class ToData)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.BidTerms (BidTerms)

newtype StandingBidState = StandingBidState (Maybe BidTerms)

derive instance Generic StandingBidState _
derive instance Newtype StandingBidState _
derive instance Eq StandingBidState
derive newtype instance ToData StandingBidState
derive newtype instance FromData StandingBidState

instance Show StandingBidState where
  show = genericShow
