module HydraAuctionOffchain.Contract.Types.Plutus.AuctionPolicyRedemeer
  ( AuctionPolicyRedeemer(MintAuction, BurnAuction)
  ) where

import Prelude

import Cardano.FromData (class FromData, genericFromData)
import Cardano.Plutus.DataSchema
  ( class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  , S
  , Z
  )
import Cardano.ToData (class ToData, genericToData)
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
