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
      , InvalidStartingBidError
      , InvalidAuctionFeePerDelegateError
      , NoDelegatesError
      )
  , auctionTermsCodec
  , auctionTermsInputCodec
  , mkAuctionTerms
  , totalAuctionFees
  , validateAuctionTerms
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Plutus.Types.Value (gt, valueToCoin) as Plutus.Value
import Cardano.Types (BigNum, Ed25519KeyHash, NetworkId)
import Contract.Numeric.BigNum (fromInt, mul, zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Time (POSIXTime)
import Data.Codec.Argonaut (JsonCodec, array, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (fold, length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Data.Validation.Semigroup (V)
import HydraAuctionOffchain.Codec
  ( bigNumCodec
  , ed25519KeyHashCodec
  , plutusAddressCodec
  , plutusValueCodec
  , posixTimeCodec
  )
import HydraAuctionOffchain.Contract.Types.VerificationKey
  ( VerificationKey
  , vkeyBytes
  , vkeyCodec
  )
import HydraAuctionOffchain.Helpers (errV)
import HydraAuctionOffchain.Lib.Crypto (hashVk)
import HydraAuctionOffchain.Lib.Plutus.Address (toPubKeyHash)
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- AuctionTermsInput
----------------------------------------------------------------------

type AuctionTermsInputRec (r :: Row Type) =
  ( auctionLot :: Plutus.Value
  , delegates :: Array Ed25519KeyHash
  , biddingStart :: POSIXTime
  , biddingEnd :: POSIXTime
  , purchaseDeadline :: POSIXTime
  , cleanup :: POSIXTime
  , auctionFeePerDelegate :: BigNum
  , startingBid :: BigNum
  , minBidIncrement :: BigNum
  , minDepositAmount :: BigNum
  | r
  )

type AuctionTermsInput = Record (AuctionTermsInputRec ())

auctionTermsInputCodec :: CA.JsonCodec AuctionTermsInput
auctionTermsInputCodec = CA.object "AuctionTerms" $ CAR.record
  { auctionLot: plutusValueCodec
  , delegates: CA.array ed25519KeyHashCodec
  , biddingStart: posixTimeCodec
  , biddingEnd: posixTimeCodec
  , purchaseDeadline: posixTimeCodec
  , cleanup: posixTimeCodec
  , auctionFeePerDelegate: bigNumCodec
  , startingBid: bigNumCodec
  , minBidIncrement: bigNumCodec
  , minDepositAmount: bigNumCodec
  }

mkAuctionTerms :: AuctionTermsInput -> Plutus.Address -> VerificationKey -> AuctionTerms
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
  ( sellerAddress :: Plutus.Address
  , sellerVk :: VerificationKey
  )

newtype AuctionTerms = AuctionTerms (Record AuctionTermsRec)

derive instance Generic AuctionTerms _
derive instance Newtype AuctionTerms _
derive instance Eq AuctionTerms

instance Show AuctionTerms where
  show = genericShow

type AuctionTermsSchema =
  ("auctionLot" :~: Plutus.Value)
    :$: ("sellerAddress" :~: Plutus.Address)
    :$: ("sellerVk" :~: VerificationKey)
    :$: ("delegates" :~: Array Ed25519KeyHash)
    :$: ("biddingStart" :~: POSIXTime)
    :$: ("biddingEnd" :~: POSIXTime)
    :$: ("purchaseDeadline" :~: POSIXTime)
    :$: ("cleanup" :~: POSIXTime)
    :$: ("auctionFeePerDelegate" :~: BigNum)
    :$: ("startingBid" :~: BigNum)
    :$: ("minBidIncrement" :~: BigNum)
    :$: ("minDepositAmount" :~: BigNum)
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

auctionTermsCodec :: NetworkId -> CA.JsonCodec AuctionTerms
auctionTermsCodec network =
  wrapIso AuctionTerms $ CA.object "AuctionTerms" $ CAR.record
    { auctionLot: plutusValueCodec
    , sellerAddress: plutusAddressCodec network
    , sellerVk: vkeyCodec
    , delegates: CA.array ed25519KeyHashCodec
    , biddingStart: posixTimeCodec
    , biddingEnd: posixTimeCodec
    , purchaseDeadline: posixTimeCodec
    , cleanup: posixTimeCodec
    , auctionFeePerDelegate: bigNumCodec
    , startingBid: bigNumCodec
    , minBidIncrement: bigNumCodec
    , minDepositAmount: bigNumCodec
    }

totalAuctionFees :: AuctionTerms -> Maybe BigNum
totalAuctionFees (AuctionTerms auctionTerms) =
  BigNum.mul (BigNum.fromInt $ length auctionTerms.delegates)
    auctionTerms.auctionFeePerDelegate

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
  | InvalidStartingBidError
  | InvalidAuctionFeePerDelegateError
  | NoDelegatesError

derive instance Generic AuctionTermsValidationError _
derive instance Eq AuctionTermsValidationError

instance Show AuctionTermsValidationError where
  show = genericShow

-- TODO: Replace with min ada for Ada-only input?
minAuctionFee :: BigNum
minAuctionFee = BigNum.fromInt 2_000_000

validateAuctionTerms :: AuctionTerms -> V (Array AuctionTermsValidationError) Unit
validateAuctionTerms auctionTerms@(AuctionTerms rec) = fold
  [ (Plutus.Value.valueToCoin rec.auctionLot == mempty)
      `errV` AuctionLotNonZeroAdaError
  , (rec.auctionLot `Plutus.Value.gt` mempty)
      `errV` NonPositiveAuctionLotValueError
  , (isJust sellerPkh)
      `errV` SellerAddressLacksPubKeyCredentialError
  , (sellerPkh == hashVk (vkeyBytes rec.sellerVk))
      `errV` SellerVkPkhMismatchError
  , (rec.biddingStart < rec.biddingEnd)
      `errV` BiddingStartNotBeforeBiddingEndError
  , (rec.biddingEnd < rec.purchaseDeadline)
      `errV` BiddingEndNotBeforePurchaseDeadlineError
  , (rec.purchaseDeadline < rec.cleanup)
      `errV` PurchaseDeadlineNotBeforeCleanupError
  , (maybe false (rec.startingBid > _) $ totalAuctionFees auctionTerms)
      `errV` InvalidStartingBidError
  , (rec.auctionFeePerDelegate > minAuctionFee)
      `errV` InvalidAuctionFeePerDelegateError
  , (length rec.delegates > 0)
      `errV` NoDelegatesError
  ]
  where
  sellerPkh :: Maybe Ed25519KeyHash
  sellerPkh = unwrap <$> toPubKeyHash rec.sellerAddress
