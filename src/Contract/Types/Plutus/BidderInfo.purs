module HydraAuctionOffchain.Contract.Types.Plutus.BidderInfo
  ( BidderInfo(BidderInfo)
  , bidderInfoCodec
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (Address, pubKeyHashAddress)
import Contract.Config (NetworkId)
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
import HydraAuctionOffchain.Codec (addressCodec)
import HydraAuctionOffchain.Contract.Types.VerificationKey
  ( VerificationKey
  , vkeyBytes
  , vkeyCodec
  )
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Lib.Crypto (hashVk)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Type.Proxy (Proxy(Proxy))

newtype BidderInfo = BidderInfo
  { bidderAddress :: Address
  , bidderVk :: VerificationKey
  }

derive instance Generic BidderInfo _
derive instance Newtype BidderInfo _
derive instance Eq BidderInfo

instance Show BidderInfo where
  show = genericShow

type BidderInfoSchema =
  ("bidderAddress" :~: Address)
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

instance HasJson BidderInfo NetworkId where
  jsonCodec network = const (bidderInfoCodec network)

bidderInfoCodec :: NetworkId -> CA.JsonCodec BidderInfo
bidderInfoCodec network =
  wrapIso BidderInfo $ CA.object "BidderInfo" $ CAR.record
    { bidderAddress: addressCodec network
    , bidderVk: vkeyCodec
    }

instance Arbitrary BidderInfo where
  arbitrary = do
    bidderVk <- arbitrary
    let
      bidderPkh = wrap $ unsafePartial fromJust $ hashVk $ vkeyBytes bidderVk
      bidderAddress = pubKeyHashAddress bidderPkh Nothing
    pure $ wrap { bidderAddress, bidderVk }
