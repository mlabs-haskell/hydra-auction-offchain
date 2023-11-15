module HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms
  ( AuctionTerms(AuctionTerms)
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (PubKeyHash)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Prim.ByteArray (ByteArray)
import Contract.Time (POSIXTime)
import Data.BigInt (BigInt)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.Extra.AssetClass (AssetClass)
import Type.Proxy (Proxy(Proxy))

newtype AuctionTerms = AuctionTerms
  { auctionLot :: AssetClass
  , sellerPkh :: PubKeyHash
  , sellerVk :: ByteArray
  , delegates :: Array PubKeyHash
  , biddingStart :: POSIXTime
  , biddingEnd :: POSIXTime
  , purchaseDeadline :: POSIXTime
  , cleanup :: POSIXTime
  , auctionFeePerDelegate :: BigInt
  , startingBid :: BigInt
  , minBidIncrement :: BigInt
  , minDepositAmount :: BigInt
  }

derive instance Generic AuctionTerms _
derive instance Newtype AuctionTerms _
derive instance Eq AuctionTerms

instance Show AuctionTerms where
  show = genericShow

type AuctionTermsSchema =
  ("auctionLot" :~: AssetClass)
    :$: ("sellerPkh" :~: PubKeyHash)
    :$: ("sellerVk" :~: ByteArray)
    :$: ("delegates" :~: Array PubKeyHash)
    :$: ("biddingStart" :~: POSIXTime)
    :$: ("biddingEnd" :~: POSIXTime)
    :$: ("purchaseDeadline" :~: POSIXTime)
    :$: ("cleanup" :~: POSIXTime)
    :$: ("auctionFeePerDelegate" :~: BigInt)
    :$: ("startingBid" :~: BigInt)
    :$: ("minBidIncrement" :~: BigInt)
    :$: ("minDepositAmount" :~: BigInt)
    :$: Nil

auctionTermsSchema :: Proxy AuctionTermsSchema
auctionTermsSchema = Proxy

instance ToData AuctionTerms where
  toData (AuctionTerms rec) = Constr BigNum.zero $ toDataRec auctionTermsSchema rec

instance FromData AuctionTerms where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: Proxy AuctionTerms) == length pd =
        wrap <$> fromDataRec auctionTermsSchema pd
  fromData _ = Nothing
