module HydraAuctionOffchain.Contract.Types.Plutus.Redeemers
  ( AuctionEscrowRedeemer
      ( StartBiddingRedeemer
      , BidderBuysRedeemer
      , SellerReclaimsRedeemer
      , CleanupAuctionRedeemer
      )
  , StandingBidRedeemer
      ( NewBidRedeemer
      , MoveToHydraRedeemer
      , ConcludeAuctionRedeemer
      )
  ) where

import Contract.PlutusData
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

--------------------------------------------------------------------------------
-- AuctionEscrow
--------------------------------------------------------------------------------

data AuctionEscrowRedeemer
  = StartBiddingRedeemer
  | BidderBuysRedeemer
  | SellerReclaimsRedeemer
  | CleanupAuctionRedeemer

derive instance Generic AuctionEscrowRedeemer _
derive instance Eq AuctionEscrowRedeemer

instance Show AuctionEscrowRedeemer where
  show = genericShow

instance
  HasPlutusSchema
    AuctionEscrowRedeemer
    ( "StartBiddingRedeemer"
        := PNil
        @@ Z
        :+ "BidderBuysRedeemer"
        := PNil
        @@ (S Z)
        :+ "SellerReclaimsRedeemer"
        := PNil
        @@ (S (S Z))
        :+ "CleanupAuctionRedeemer"
        := PNil
        @@ (S (S (S (Z))))
        :+ PNil
    )

instance ToData AuctionEscrowRedeemer where
  toData = genericToData

instance FromData AuctionEscrowRedeemer where
  fromData = genericFromData

----------------------------------------------------------------------
-- StandingBid
----------------------------------------------------------------------

data StandingBidRedeemer
  = NewBidRedeemer
  | MoveToHydraRedeemer
  | ConcludeAuctionRedeemer

derive instance Generic StandingBidRedeemer _
derive instance Eq StandingBidRedeemer

instance Show StandingBidRedeemer where
  show = genericShow

instance
  HasPlutusSchema
    StandingBidRedeemer
    ( "NewBidRedeemer"
        := PNil
        @@ Z
        :+ "MoveToHydraRedeemer"
        := PNil
        @@ (S Z)
        :+ "ConcludeAuctionRedeemer"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData StandingBidRedeemer where
  toData = genericToData

instance FromData StandingBidRedeemer where
  fromData = genericFromData
