module HydraAuctionOffchain.Contract.Types.Plutus.BidTerms
  ( BidTerms(BidTerms)
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Prim.ByteArray (ByteArray)
import Data.BigInt (BigInt)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.BidderInfo (BidderInfo)
import Type.Proxy (Proxy(Proxy))

newtype BidTerms = BidTerms
  { bidder :: BidderInfo
  , price :: BigInt
  , bidderSignature :: ByteArray
  , sellerSignature :: ByteArray
  }

derive instance Generic BidTerms _
derive instance Newtype BidTerms _
derive instance Eq BidTerms

instance Show BidTerms where
  show = genericShow

type BidTermsSchema =
  ("bidder" :~: BidderInfo)
    :$: ("price" :~: BigInt)
    :$: ("bidderSignature" :~: ByteArray)
    :$: ("sellerSignature" :~: ByteArray)
    :$: Nil

bidTermsSchema :: Proxy BidTermsSchema
bidTermsSchema = Proxy

instance ToData BidTerms where
  toData (BidTerms rec) = Constr BigNum.zero $ toDataRec bidTermsSchema rec

instance FromData BidTerms where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: Proxy BidTerms) == length pd =
        wrap <$> fromDataRec bidTermsSchema pd
  fromData _ = Nothing