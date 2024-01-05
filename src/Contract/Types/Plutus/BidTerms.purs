module HydraAuctionOffchain.Contract.Types.Plutus.BidTerms
  ( BidTerms(BidTerms)
  , bidderSignatureMessage
  , sellerSignatureMessage
  , validateBidTerms
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (PubKeyHash)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr), serializeData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Plutus.BidderInfo (BidderInfo)
import Type.Proxy (Proxy(Proxy))
import Undefined (undefined)

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

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateBidTerms :: CurrencySymbol -> AuctionTerms -> BidTerms -> Boolean
validateBidTerms = undefined

bidderSignatureMessage :: CurrencySymbol -> PubKeyHash -> BigInt -> ByteArray
bidderSignatureMessage auctionCs bidderPkh bidPrice =
  unwrap $ serializeData auctionCs <> serializeData bidderPkh <> serializeData bidPrice

sellerSignatureMessage :: CurrencySymbol -> ByteArray -> ByteArray
sellerSignatureMessage auctionCs bidderVk =
  unwrap $ serializeData auctionCs <> serializeData bidderVk
