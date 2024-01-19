module HydraAuctionOffchain.Contract.Types.Plutus.BidTerms
  ( BidTerms(BidTerms)
  , bidTermsCodec
  , bidderSignatureMessage
  , sellerSignatureMessage
  , validateBidTerms
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (PubKeyHash, toPubKeyHash)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr), serializeData)
import Contract.Prim.ByteArray (ByteArray, byteLength, hexToByteArrayUnsafe)
import Contract.Value (CurrencySymbol)
import Data.Array (fold)
import Data.Array (replicate) as Array
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import HydraAuctionOffchain.Codec (class HasJson, bigIntCodec, byteArrayCodec)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms(AuctionTerms))
import HydraAuctionOffchain.Contract.Types.Plutus.BidderInfo (BidderInfo, bidderInfoCodec)
import HydraAuctionOffchain.Contract.Types.VerificationKey (vkeyBytes)
import HydraAuctionOffchain.Lib.Cose (mkSigStructure)
import HydraAuctionOffchain.Lib.Crypto (verifySignature)
import JS.BigInt (BigInt)
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

instance HasJson BidTerms where
  jsonCodec = const bidTermsCodec

bidTermsCodec :: CA.JsonCodec BidTerms
bidTermsCodec =
  wrapIso BidTerms $ CA.object "BidTerms" $ CAR.record
    { bidder: bidderInfoCodec
    , price: bigIntCodec
    , bidderSignature: byteArrayCodec
    , sellerSignature: byteArrayCodec
    }

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateBidTerms :: CurrencySymbol -> AuctionTerms -> BidTerms -> Effect Boolean
validateBidTerms auctionCs (AuctionTerms auctionTerms) (BidTerms bidTerms) =
  conj <$> verifySellerSignature <*> verifyBidderSignature
  where
  bidderInfo = unwrap bidTerms.bidder

  verifyBidderSignature :: Effect Boolean
  verifyBidderSignature =
    case bidderInfo.bidderAddress of
      bidderAddress | Just bidderPkh <- toPubKeyHash bidderAddress -> do
        let payload = bidderSignatureMessage auctionCs bidderPkh bidTerms.price
        sigStruct <- mkSigStructure bidderAddress payload
        verifySignature (vkeyBytes bidderInfo.bidderVk) sigStruct bidTerms.bidderSignature
      _ -> pure false

  verifySellerSignature :: Effect Boolean
  verifySellerSignature = do
    let payload = sellerSignatureMessage auctionCs $ vkeyBytes bidderInfo.bidderVk
    sigStruct <- mkSigStructure auctionTerms.sellerAddress payload
    verifySignature (vkeyBytes auctionTerms.sellerVk) sigStruct bidTerms.sellerSignature

bidderSignatureMessageSize :: Int
bidderSignatureMessageSize = 69

bidderSignatureMessage :: CurrencySymbol -> PubKeyHash -> BigInt -> ByteArray
bidderSignatureMessage auctionCs bidderPkh bidPrice =
  padMessage bidderSignatureMessageSize $ unwrap
    (serializeData auctionCs <> serializeData bidderPkh <> serializeData bidPrice)

sellerSignatureMessage :: CurrencySymbol -> ByteArray -> ByteArray
sellerSignatureMessage auctionCs bidderVk =
  unwrap $ serializeData auctionCs <> serializeData bidderVk

padMessage :: Int -> ByteArray -> ByteArray
padMessage targetSize message =
  if padSize <= zero then message
  else hexToByteArrayUnsafe (fold $ Array.replicate padSize "00") <> message
  where
  padSize :: Int
  padSize = targetSize - byteLength message
