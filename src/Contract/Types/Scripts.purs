module HydraAuctionOffchain.Contract.Types.Scripts
  ( AuctionEscrowScriptHash(AuctionEscrowScriptHash)
  , FeeEscrowScriptHash(FeeEscrowScriptHash)
  , StandingBidScriptHash(StandingBidScriptHash)
  ) where

import Prelude

import Contract.PlutusData (class ToData)
import Contract.Scripts (ScriptHash)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Ply.Typename (class PlyTypeName)

----------------------------------------------------------------------
-- AuctionEscrow

newtype AuctionEscrowScriptHash = AuctionEscrowScriptHash ScriptHash

derive instance Generic AuctionEscrowScriptHash _
derive instance Newtype AuctionEscrowScriptHash _
derive newtype instance ToData AuctionEscrowScriptHash

instance Show AuctionEscrowScriptHash where
  show = genericShow

instance PlyTypeName AuctionEscrowScriptHash where
  plyTypeName _ = "HydraAuctionOnchain.Types.Scripts:AuctionEscrowScriptHash"

----------------------------------------------------------------------
-- StandingBid

newtype StandingBidScriptHash = StandingBidScriptHash ScriptHash

derive instance Generic StandingBidScriptHash _
derive instance Newtype StandingBidScriptHash _
derive newtype instance ToData StandingBidScriptHash

instance Show StandingBidScriptHash where
  show = genericShow

instance PlyTypeName StandingBidScriptHash where
  plyTypeName _ = "HydraAuctionOnchain.Types.Scripts:StandingBidScriptHash"

----------------------------------------------------------------------
-- FeeEscrow

newtype FeeEscrowScriptHash = FeeEscrowScriptHash ScriptHash

derive instance Generic FeeEscrowScriptHash _
derive instance Newtype FeeEscrowScriptHash _
derive newtype instance ToData FeeEscrowScriptHash

instance Show FeeEscrowScriptHash where
  show = genericShow

instance PlyTypeName FeeEscrowScriptHash where
  plyTypeName _ = "HydraAuctionOnchain.Types.Scripts:FeeEscrowScriptHash"
