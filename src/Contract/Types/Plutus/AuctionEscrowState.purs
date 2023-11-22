module HydraAuctionOffchain.Contract.Types.Plutus.AuctionEscrowState
  ( AuctionEscrowState(AuctionAnnounced, BiddingStarted, AuctionConcluded)
  ) where

import Contract.PlutusData
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data AuctionEscrowState
  = AuctionAnnounced
  | BiddingStarted
  | AuctionConcluded

derive instance Generic AuctionEscrowState _
derive instance Eq AuctionEscrowState

instance Show AuctionEscrowState where
  show = genericShow

instance
  HasPlutusSchema
    AuctionEscrowState
    ( "AuctionAnnounced"
        := PNil
        @@ Z
        :+ "BiddingStarted"
        := PNil
        @@ (S Z)
        :+ "AuctionConcluded"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData AuctionEscrowState where
  toData = genericToData

instance FromData AuctionEscrowState where
  fromData = genericFromData
