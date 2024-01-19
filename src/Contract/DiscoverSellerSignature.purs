module HydraAuctionOffchain.Contract.DiscoverSellerSignature
  ( DiscoverSellerSigContractError
      ( DiscoverSellerSig_Error_CouldNotGetSellerPubKeyHash
      , DiscoverSellerSig_Error_CouldNotGetBidderPubKey
      )
  , DiscoverSellerSigContractParams(DiscoverSellerSigContractParams)
  , discoverSellerSignature
  ) where

import Contract.Prelude

import Contract.Address (Address, PubKeyHash, scriptHashAddress, toPubKeyHash)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(Datum), OutputDatum(OutputDatum), fromData)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.Transaction (TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Control.Error.Util (bool, (??))
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, findMap) as Array
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (class HasJson, addressCodec, currencySymbolCodec)
import HydraAuctionOffchain.Config (config)
import HydraAuctionOffchain.Contract.PersonalOracle (PersonalOracle, mkPersonalOracle)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionAuth(AuctionAuth)
  , ContractOutput
  , VerificationKey
  , mkContractOutput
  )
import HydraAuctionOffchain.Wallet (SignMessageError, signMessage)
import Partial.Unsafe (unsafePartial)

newtype DiscoverSellerSigContractParams = DiscoverSellerSigContractParams
  { auctionCs :: CurrencySymbol
  , sellerAddress :: Address
  }

derive instance Generic DiscoverSellerSigContractParams _
derive instance Newtype DiscoverSellerSigContractParams _
derive instance Eq DiscoverSellerSigContractParams

instance Show DiscoverSellerSigContractParams where
  show = genericShow

instance HasJson DiscoverSellerSigContractParams where
  jsonCodec = const discoverSellerSigContractParamsCodec

discoverSellerSigContractParamsCodec :: CA.JsonCodec DiscoverSellerSigContractParams
discoverSellerSigContractParamsCodec =
  wrapIso DiscoverSellerSigContractParams $ CA.object "DiscoverSellerSigContractParams" $
    CAR.record
      { auctionCs: currencySymbolCodec
      , sellerAddress: addressCodec config.network
      }

discoverSellerSignature
  :: DiscoverSellerSigContractParams
  -> Contract (ContractOutput (Maybe ByteArray))
discoverSellerSignature =
  mkContractOutput identity <<< discoverSellerSignatureWithErrors

discoverSellerSignatureWithErrors
  :: DiscoverSellerSigContractParams
  -> ExceptT DiscoverSellerSigContractError Contract (Maybe ByteArray)
discoverSellerSignatureWithErrors (DiscoverSellerSigContractParams params) = do
  -- Get seller pkh:
  sellerPkh <- toPubKeyHash params.sellerAddress
    ?? DiscoverSellerSig_Error_CouldNotGetSellerPubKeyHash

  -- Get bidder vk:
  let signAsciiMessage = signMessage <<< unsafePartial fromJust <<< byteArrayFromAscii
  { vkey: bidderVk } <-
    withExceptT DiscoverSellerSig_Error_CouldNotGetBidderPubKey $
      signAsciiMessage
        "By signing this message, you authorize hydra-auction to read \
        \your public key."

  lift $ findSignature sellerPkh params.auctionCs bidderVk

findSignature :: PubKeyHash -> CurrencySymbol -> VerificationKey -> Contract (Maybe ByteArray)
findSignature sellerPkh auctionCs bidderVk = do
  utxos <- utxosAt sellerOracleAddr
  let txOuts = Map.toUnfoldable utxos <#> _.output <<< unwrap <<< snd
  pure $ Array.findMap worker txOuts
  where
  sellerOracle :: PersonalOracle
  sellerOracle = mkPersonalOracle $ wrap sellerPkh

  sellerOracleAddr :: Address
  sellerOracleAddr = scriptHashAddress (wrap sellerOracle.nativeScriptHash) Nothing

  worker :: TransactionOutput -> Maybe ByteArray
  worker txOut = do
    AuctionAuth auctionAuth <- getAuctionAuth
    snd <$>
      bool Nothing (Array.find (eq bidderVk <<< fst) auctionAuth.signatures)
        (auctionAuth.auctionCs == auctionCs)
    where
    getAuctionAuth :: Maybe AuctionAuth
    getAuctionAuth =
      case (unwrap txOut).datum of
        OutputDatum (Datum plutusData) -> fromData plutusData
        _ -> Nothing

----------------------------------------------------------------------
-- Errors

data DiscoverSellerSigContractError
  = DiscoverSellerSig_Error_CouldNotGetSellerPubKeyHash
  | DiscoverSellerSig_Error_CouldNotGetBidderPubKey SignMessageError

derive instance Generic DiscoverSellerSigContractError _
derive instance Eq DiscoverSellerSigContractError

instance Show DiscoverSellerSigContractError where
  show = genericShow

instance ToContractError DiscoverSellerSigContractError where
  toContractError = wrap <<< case _ of
    DiscoverSellerSig_Error_CouldNotGetSellerPubKeyHash ->
      { errorCode: "DiscoverSellerSignature01"
      , message: "Could not get seller pub key hash from address."
      }
    DiscoverSellerSig_Error_CouldNotGetBidderPubKey err ->
      { errorCode: "DiscoverSellerSignature02"
      , message: "Could not get bidder pub key, error: " <> show err <> "."
      }
