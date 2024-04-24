module HydraAuctionOffchain.Contract.Types.AuctionFilters
  ( AuctionFilters(AuctionFilters)
  , auctionFiltersCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (optional, record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionActor (ActorRole, actorRoleCodec)
import HydraAuctionOffchain.Lib.Codec (class HasJson)

newtype AuctionFilters = AuctionFilters
  { myRole :: Maybe ActorRole
  }

derive instance Generic AuctionFilters _
derive instance Newtype AuctionFilters _
derive instance Eq AuctionFilters

instance Show AuctionFilters where
  show = genericShow

instance HasJson AuctionFilters anyParams where
  jsonCodec _ = const auctionFiltersCodec

auctionFiltersCodec :: CA.JsonCodec AuctionFilters
auctionFiltersCodec =
  wrapIso AuctionFilters $ CA.object "AuctionFilters" $ CAR.record
    { myRole: CAR.optional actorRoleCodec
    }
