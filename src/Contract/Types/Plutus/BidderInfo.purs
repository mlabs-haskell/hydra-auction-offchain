module HydraAuctionOffchain.Contract.Types.Plutus.BidderInfo
  ( BidderInfo(BidderInfo)
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (PubKeyHash)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Prim.ByteArray (ByteArray)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(Proxy))

newtype BidderInfo = BidderInfo
  { bidderPkh :: PubKeyHash
  , bidderVk :: ByteArray
  }

derive instance Generic BidderInfo _
derive instance Newtype BidderInfo _
derive instance Eq BidderInfo

instance Show BidderInfo where
  show = genericShow

type BidderInfoSchema =
  ("bidderPkh" :~: PubKeyHash)
    :$: ("bidderVk" :~: ByteArray)
    :$: Nil

bidderInfoSchema :: Proxy BidderInfoSchema
bidderInfoSchema = Proxy

instance ToData BidderInfo where
  toData (BidderInfo rec) = Constr BigNum.zero $ toDataRec bidderInfoSchema rec

instance FromData BidderInfo where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: Proxy BidderInfo) == length pd =
        wrap <$> fromDataRec bidderInfoSchema pd
  fromData _ = Nothing
