module HydraAuctionOffchain.Contract.Types.Plutus.BidderInfo
  ( BidderInfo(BidderInfo)
  , bidderInfoCodec
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (PubKeyHash)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (class Newtype, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (class HasJson, pubKeyHashCodec)
import HydraAuctionOffchain.Contract.Types.VerificationKey
  ( VerificationKey
  , vkeyBytes
  , vkeyCodec
  )
import HydraAuctionOffchain.Lib.Crypto (hashVk)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Type.Proxy (Proxy(Proxy))

newtype BidderInfo = BidderInfo
  { bidderPkh :: PubKeyHash
  , bidderVk :: VerificationKey
  }

derive instance Generic BidderInfo _
derive instance Newtype BidderInfo _
derive instance Eq BidderInfo

instance Show BidderInfo where
  show = genericShow

type BidderInfoSchema =
  ("bidderPkh" :~: PubKeyHash)
    :$: ("bidderVk" :~: VerificationKey)
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

instance HasJson BidderInfo where
  jsonCodec = const bidderInfoCodec

bidderInfoCodec :: CA.JsonCodec BidderInfo
bidderInfoCodec =
  wrapIso BidderInfo $ CA.object "BidderInfo" $ CAR.record
    { bidderPkh: pubKeyHashCodec
    , bidderVk: vkeyCodec
    }

instance Arbitrary BidderInfo where
  arbitrary = do
    bidderVk <- arbitrary
    let bidderPkh = unsafePartial fromJust $ hashVk $ vkeyBytes bidderVk
    pure $ wrap { bidderPkh, bidderVk }
