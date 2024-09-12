module HydraAuctionOffchain.Contract.Types.Plutus.AuctionActor
  ( ActorRole(ActorRoleSeller, ActorRoleBidder)
  , AuctionActor(AuctionActor)
  , actorRoleCodec
  ) where

import Contract.PlutusData
import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Cardano.Plutus.DataSchema (PNil, S, Z)
import Contract.Numeric.BigNum (zero) as BigNum
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionInfo (AuctionInfoExtended)
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- AuctionActor

newtype AuctionActor = AuctionActor
  { auctionInfo :: AuctionInfoExtended
  , role :: ActorRole
  }

derive instance Generic AuctionActor _
derive instance Newtype AuctionActor _
derive instance Eq AuctionActor

instance Show AuctionActor where
  show = genericShow

type AuctionActorSchema =
  ("auctionInfo" :~: AuctionInfoExtended)
    :$: ("role" :~: ActorRole)
    :$: Nil

auctionActorSchema :: Proxy AuctionActorSchema
auctionActorSchema = Proxy

instance ToData AuctionActor where
  toData (AuctionActor rec) = Constr BigNum.zero $ toDataRec auctionActorSchema rec

instance FromData AuctionActor where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: Proxy AuctionActor) == length pd =
        wrap <$> fromDataRec auctionActorSchema pd
  fromData _ = Nothing

----------------------------------------------------------------------
-- ActorRole

data ActorRole = ActorRoleSeller | ActorRoleBidder

derive instance Generic ActorRole _
derive instance Eq ActorRole

instance Show ActorRole where
  show = genericShow

instance
  HasPlutusSchema
    ActorRole
    ( "ActorRoleSeller"
        := PNil
        @@ Z
        :+ "ActorRoleBidder"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData ActorRole where
  toData = genericToData

instance FromData ActorRole where
  fromData = genericFromData

actorRoleCodec :: CA.JsonCodec ActorRole
actorRoleCodec = CAG.nullarySum "ActorRole"
