module HydraAuctionOffchain.Contract.Types.Plutus.AuctionPolicyRedemeer
  ( AuctionPolicyRedeemer(MintAuction, BurnAuction)
  ) where

import Contract.PlutusData
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data AuctionPolicyRedeemer = MintAuction | BurnAuction

derive instance Generic AuctionPolicyRedeemer _
derive instance Eq AuctionPolicyRedeemer

instance Show AuctionPolicyRedeemer where
  show = genericShow

instance
  HasPlutusSchema
    AuctionPolicyRedeemer
    ( "MintAuction"
        := PNil
        @@ Z
        :+ "BurnAuction"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData AuctionPolicyRedeemer where
  toData = genericToData

instance FromData AuctionPolicyRedeemer where
  fromData = genericFromData
