module HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms
  ( AuctionTerms(AuctionTerms)
  , AuctionTermsInput
  , AuctionTermsInputRec
  , AuctionTermsRec
  , AuctionTermsValidationError
      ( AuctionLotNonZeroAdaError
      , NonPositiveAuctionLotValueError
      , SellerAddressLacksPubKeyCredentialError
      , SellerVkPkhMismatchError
      , BiddingStartNotBeforeBiddingEndError
      , BiddingEndNotBeforePurchaseDeadlineError
      , PurchaseDeadlineNotBeforeCleanupError
      , NonPositiveMinBidIncrementError
      , InvalidStartingBidError
      , InvalidAuctionFeePerDelegateError
      , NoDelegatesError
      )
  , auctionTermsCodec
  , auctionTermsInputCodec
  , mkAuctionTerms
  , validateAuctionTerms
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (Address, PubKeyHash, toPubKeyHash)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Time (POSIXTime)
import Contract.Value (Value, valueToCoin)
import Contract.Value (gt) as Value
import Data.Codec.Argonaut (JsonCodec, array, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (fold, length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), isJust)
import Data.Newtype (class Newtype, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Data.Validation.Semigroup (V)
import HydraAuctionOffchain.Codec
  ( addressCodec
  , bigIntCodec
  , posixTimeCodec
  , pubKeyHashCodec
  , valueCodec
  )
import HydraAuctionOffchain.Config (config)
import HydraAuctionOffchain.Contract.Types.VerificationKey
  ( VerificationKey
  , vkeyBytes
  , vkeyCodec
  )
import HydraAuctionOffchain.Helpers (errV)
import HydraAuctionOffchain.Lib.Crypto (hashVk)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import Ply.Typename (class PlyTypeName)
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- AuctionTermsInput
----------------------------------------------------------------------

type AuctionTermsInputRec (r :: Row Type) =
  ( auctionLot :: Value
  , delegates :: Array PubKeyHash
  , biddingStart :: POSIXTime
  , biddingEnd :: POSIXTime
  , purchaseDeadline :: POSIXTime
  , cleanup :: POSIXTime
  , auctionFeePerDelegate :: BigInt
  , startingBid :: BigInt
  , minBidIncrement :: BigInt
  , minDepositAmount :: BigInt
  | r
  )

type AuctionTermsInput = Record (AuctionTermsInputRec ())

auctionTermsInputCodec :: CA.JsonCodec AuctionTermsInput
auctionTermsInputCodec = CA.object "AuctionTerms" $ CAR.record
  { auctionLot: valueCodec
  , delegates: CA.array pubKeyHashCodec
  , biddingStart: posixTimeCodec
  , biddingEnd: posixTimeCodec
  , purchaseDeadline: posixTimeCodec
  , cleanup: posixTimeCodec
  , auctionFeePerDelegate: bigIntCodec
  , startingBid: bigIntCodec
  , minBidIncrement: bigIntCodec
  , minDepositAmount: bigIntCodec
  }

mkAuctionTerms :: AuctionTermsInput -> Address -> VerificationKey -> AuctionTerms
mkAuctionTerms rec sellerAddress sellerVk = wrap
  { auctionLot: rec.auctionLot
  , sellerAddress
  , sellerVk
  , delegates: rec.delegates
  , biddingStart: rec.biddingStart
  , biddingEnd: rec.biddingEnd
  , purchaseDeadline: rec.purchaseDeadline
  , cleanup: rec.cleanup
  , auctionFeePerDelegate: rec.auctionFeePerDelegate
  , startingBid: rec.startingBid
  , minBidIncrement: rec.minBidIncrement
  , minDepositAmount: rec.minDepositAmount
  }

----------------------------------------------------------------------
-- AuctionTerms
----------------------------------------------------------------------

type AuctionTermsRec = AuctionTermsInputRec
  ( sellerAddress :: Address
  , sellerVk :: VerificationKey
  )

newtype AuctionTerms = AuctionTerms (Record AuctionTermsRec)

derive instance Generic AuctionTerms _
derive instance Newtype AuctionTerms _
derive instance Eq AuctionTerms

instance Show AuctionTerms where
  show = genericShow

type AuctionTermsSchema =
  ("auctionLot" :~: Value)
    :$: ("sellerAddress" :~: Address)
    :$: ("sellerVk" :~: VerificationKey)
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

instance PlyTypeName AuctionTerms where
  plyTypeName _ = "HydraAuctionOnchain.Types.AuctionTerms:AuctionTerms"

auctionTermsCodec :: CA.JsonCodec AuctionTerms
auctionTermsCodec =
  wrapIso AuctionTerms $ CA.object "AuctionTerms" $ CAR.record
    { auctionLot: valueCodec
    , sellerAddress: addressCodec config.network
    , sellerVk: vkeyCodec
    , delegates: CA.array pubKeyHashCodec
    , biddingStart: posixTimeCodec
    , biddingEnd: posixTimeCodec
    , purchaseDeadline: posixTimeCodec
    , cleanup: posixTimeCodec
    , auctionFeePerDelegate: bigIntCodec
    , startingBid: bigIntCodec
    , minBidIncrement: bigIntCodec
    , minDepositAmount: bigIntCodec
    }

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

data AuctionTermsValidationError
  = AuctionLotNonZeroAdaError
  | NonPositiveAuctionLotValueError
  | SellerAddressLacksPubKeyCredentialError
  | SellerVkPkhMismatchError
  | BiddingStartNotBeforeBiddingEndError
  | BiddingEndNotBeforePurchaseDeadlineError
  | PurchaseDeadlineNotBeforeCleanupError
  | NonPositiveMinBidIncrementError
  | InvalidStartingBidError
  | InvalidAuctionFeePerDelegateError
  | NoDelegatesError

derive instance Generic AuctionTermsValidationError _
derive instance Eq AuctionTermsValidationError

instance Show AuctionTermsValidationError where
  show = genericShow

-- TODO: Replace with min ada for Ada-only input?
minAuctionFee :: BigInt
minAuctionFee = BigInt.fromInt 2_000_000

validateAuctionTerms :: AuctionTerms -> V (Array AuctionTermsValidationError) Unit
validateAuctionTerms (AuctionTerms rec) = fold
  [ (valueToCoin rec.auctionLot == mempty)
      `errV` AuctionLotNonZeroAdaError
  , (rec.auctionLot `Value.gt` mempty)
      `errV` NonPositiveAuctionLotValueError
  , (isJust $ toPubKeyHash rec.sellerAddress)
      `errV` SellerAddressLacksPubKeyCredentialError
  , (toPubKeyHash rec.sellerAddress == hashVk (vkeyBytes rec.sellerVk))
      `errV` SellerVkPkhMismatchError
  , (rec.biddingStart < rec.biddingEnd)
      `errV` BiddingStartNotBeforeBiddingEndError
  , (rec.biddingEnd < rec.purchaseDeadline)
      `errV` BiddingEndNotBeforePurchaseDeadlineError
  , (rec.purchaseDeadline < rec.cleanup)
      `errV` PurchaseDeadlineNotBeforeCleanupError
  , (rec.minBidIncrement > zero)
      `errV` NonPositiveMinBidIncrementError
  , (rec.startingBid > rec.auctionFeePerDelegate * length rec.delegates)
      `errV` InvalidStartingBidError
  , (rec.auctionFeePerDelegate > minAuctionFee)
      `errV` InvalidAuctionFeePerDelegateError
  , (length rec.delegates > 0)
      `errV` NoDelegatesError
  ]
