module HydraAuctionOffchain.Contract.Types.Plutus.BidTerms
  ( BidTerms(BidTerms)
  , bidTermsCodec
  , bidderSignatureMessage
  , sellerPayout
  , sellerSignatureMessage
  , validateBidTerms
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(PubKeyHash)) as Plutus
import Cardano.Types
  ( BigNum
  , Credential(PubKeyHashCredential)
  , Ed25519KeyHash
  , NetworkId
  , ScriptHash
  )
import Cardano.Types.Address (mkPaymentAddress)
import Contract.Numeric.BigNum (sub, zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Prim.ByteArray (ByteArray, byteLength, hexToByteArrayUnsafe)
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
import HydraAuctionOffchain.Codec (bigIntCodec, bigNumCodec, byteArrayCodec)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms
  ( AuctionTerms(AuctionTerms)
  , totalAuctionFees
  )
import HydraAuctionOffchain.Contract.Types.Plutus.BidderInfo (BidderInfo, bidderInfoCodec)
import HydraAuctionOffchain.Contract.Types.VerificationKey (vkeyBytes)
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Lib.Cose (mkSigStructure)
import HydraAuctionOffchain.Lib.Crypto (verifySignature)
import HydraAuctionOffchain.Lib.Plutus.Address (toPubKeyHash)
import HydraAuctionOffchain.Lib.ToData (serializeData)
import JS.BigInt (BigInt)
import Type.Proxy (Proxy(Proxy))

newtype BidTerms = BidTerms
  { bidder :: BidderInfo
  , price :: BigNum
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
    :$: ("price" :~: BigNum)
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

instance HasJson BidTerms NetworkId where
  jsonCodec network = const (bidTermsCodec network)

bidTermsCodec :: NetworkId -> CA.JsonCodec BidTerms
bidTermsCodec network =
  wrapIso BidTerms $ CA.object "BidTerms" $ CAR.record
    { bidder: bidderInfoCodec network
    , price: bigNumCodec
    , bidderSignature: byteArrayCodec
    , sellerSignature: byteArrayCodec
    }

sellerPayout :: AuctionTerms -> BidTerms -> Maybe BigNum
sellerPayout auctionTerms bidTerms =
  BigNum.sub (unwrap bidTerms).price =<< totalAuctionFees auctionTerms

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateBidTerms :: NetworkId -> ScriptHash -> AuctionTerms -> BidTerms -> Effect Boolean
validateBidTerms network auctionCs (AuctionTerms auctionTerms) (BidTerms bidTerms) =
  conj <$> verifySellerSignature <*> verifyBidderSignature
  where
  bidderInfo = unwrap bidTerms.bidder

  verifyBidderSignature :: Effect Boolean
  verifyBidderSignature =
    case toPubKeyHash bidderInfo.bidderAddress of
      Just (Plutus.PubKeyHash bidderPkh) -> do
        let
          addr = mkPaymentAddress network (wrap $ PubKeyHashCredential bidderPkh) Nothing
          payload = bidderSignatureMessage auctionCs bidderPkh bidTerms.price
        sigStruct <- mkSigStructure addr payload
        verifySignature (vkeyBytes bidderInfo.bidderVk) sigStruct bidTerms.bidderSignature
      Nothing -> pure false

  verifySellerSignature :: Effect Boolean
  verifySellerSignature =
    case toPubKeyHash auctionTerms.sellerAddress of
      Just (Plutus.PubKeyHash sellerPkh) -> do
        let
          addr = mkPaymentAddress network (wrap $ PubKeyHashCredential sellerPkh) Nothing
          payload = sellerSignatureMessage auctionCs $ vkeyBytes bidderInfo.bidderVk
        sigStruct <- mkSigStructure addr payload
        verifySignature (vkeyBytes auctionTerms.sellerVk) sigStruct bidTerms.sellerSignature
      Nothing -> pure false

-- Maximum (reasonable) size of the bidder signature message where
-- bidPrice is set to the total supply of ADA (45 billion). 
-- 
-- Note, that the bid price is the only component of the message that 
-- has variable size; and for lower bid prices the message is padded 
-- with zero bytes at the beginning to reach this size.
bidderSignatureMessageSize :: Int
bidderSignatureMessageSize = 69

bidderSignatureMessage :: ScriptHash -> Ed25519KeyHash -> BigNum -> ByteArray
bidderSignatureMessage auctionCs bidderPkh bidPrice =
  padMessage bidderSignatureMessageSize $ unwrap
    (serializeData auctionCs <> serializeData bidderPkh <> serializeData bidPrice)

sellerSignatureMessage :: ScriptHash -> ByteArray -> ByteArray
sellerSignatureMessage auctionCs bidderVk =
  unwrap $ serializeData auctionCs <> serializeData bidderVk

padMessage :: Int -> ByteArray -> ByteArray
padMessage targetSize message =
  if padSize <= zero then message
  else hexToByteArrayUnsafe (fold $ Array.replicate padSize "00") <> message
  where
  padSize :: Int
  padSize = targetSize - byteLength message
