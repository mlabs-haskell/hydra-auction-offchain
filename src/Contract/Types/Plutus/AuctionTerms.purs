module HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms
  ( AuctionTerms(AuctionTerms)
  , AuctionTermsInput
  , AuctionTermsInputRec
  , AuctionTermsRec
  , AuctionTermsValidationError
      ( NonPositiveAuctionLotValueError
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

import Contract.Address (PubKeyHash)
import Contract.Hashing (blake2b224Hash)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Prim.ByteArray (ByteArray)
import Contract.Time (POSIXTime)
import Contract.Value (Value)
import Contract.Value (gt) as Value
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Codec.Argonaut (JsonCodec, array, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (fold, length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Data.Validation.Semigroup (V, invalid)
import HydraAuctionOffchain.Codec
  ( bigIntCodec
  , byteArrayCodec
  , posixTimeCodec
  , pubKeyHashCodec
  , valueCodec
  )
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

mkAuctionTerms :: AuctionTermsInput -> PubKeyHash -> ByteArray -> AuctionTerms
mkAuctionTerms rec sellerPkh sellerVk = wrap
  { auctionLot: rec.auctionLot
  , sellerPkh
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
  ( sellerPkh :: PubKeyHash
  , sellerVk :: ByteArray
  )

newtype AuctionTerms = AuctionTerms (Record AuctionTermsRec)

derive instance Generic AuctionTerms _
derive instance Newtype AuctionTerms _
derive instance Eq AuctionTerms

instance Show AuctionTerms where
  show = genericShow

type AuctionTermsSchema =
  ("auctionLot" :~: Value)
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

instance PlyTypeName AuctionTerms where
  plyTypeName _ = "HydraAuctionOnchain.Types.AuctionTerms:AuctionTerms"

auctionTermsCodec :: CA.JsonCodec AuctionTerms
auctionTermsCodec =
  wrapIso AuctionTerms $ CA.object "AuctionTerms" $ CAR.record
    { auctionLot: valueCodec
    , sellerPkh: pubKeyHashCodec
    , sellerVk: byteArrayCodec
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
  = NonPositiveAuctionLotValueError
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
  [ (rec.auctionLot `Value.gt` mempty)
      `err` NonPositiveAuctionLotValueError
  , (Just rec.sellerPkh == map wrap (ed25519KeyHashFromBytes $ blake2b224Hash rec.sellerVk))
      `err` SellerVkPkhMismatchError
  , (rec.biddingStart < rec.biddingEnd)
      `err` BiddingStartNotBeforeBiddingEndError
  , (rec.biddingEnd < rec.purchaseDeadline)
      `err` BiddingEndNotBeforePurchaseDeadlineError
  , (rec.purchaseDeadline < rec.cleanup)
      `err` PurchaseDeadlineNotBeforeCleanupError
  , (rec.minBidIncrement > zero)
      `err` NonPositiveMinBidIncrementError
  , (rec.startingBid > rec.auctionFeePerDelegate * length rec.delegates)
      `err` InvalidStartingBidError
  , (rec.auctionFeePerDelegate > minAuctionFee)
      `err` InvalidAuctionFeePerDelegateError
  , (length rec.delegates > 0)
      `err` NoDelegatesError
  ]
  where
  err :: Boolean -> AuctionTermsValidationError -> V (Array AuctionTermsValidationError) Unit
  err x error = if x then pure unit else invalid [ error ]
