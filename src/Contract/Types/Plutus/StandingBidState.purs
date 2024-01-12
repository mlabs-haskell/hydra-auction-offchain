module HydraAuctionOffchain.Contract.Types.Plutus.StandingBidState
  ( StandingBidState(StandingBidState)
  , standingBidStateCodec
  ) where

import Prelude

import Contract.PlutusData (class FromData, class ToData)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (class HasJson)
import HydraAuctionOffchain.Contract.Types.Plutus.BidTerms (BidTerms, bidTermsCodec)

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
