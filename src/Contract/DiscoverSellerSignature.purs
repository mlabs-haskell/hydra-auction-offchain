module HydraAuctionOffchain.Contract.DiscoverSellerSignature
  ( DiscoverSellerSigContractError
      ( DiscoverSellerSig_Error_CouldNotGetSellerPubKeyHash
      , DiscoverSellerSig_Error_CouldNotGetBidderPubKey
      )
  , DiscoverSellerSigContractParams(DiscoverSellerSigContractParams)
  , discoverSellerSignature
  , discoverSellerSignatureWithErrors
  ) where

import Contract.Prelude

import Contract.Address (Address, PubKeyHash, scriptHashAddress, toPubKeyHash)
import Contract.Config (NetworkId)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Control.Error.Util (bool, (??))
import Control.Monad.Except (ExceptT, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, findMap) as Array
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Map (toUnfoldable) as Map
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (addressCodec, currencySymbolCodec)
import HydraAuctionOffchain.Contract.PersonalOracle (PersonalOracle, mkPersonalOracle)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionAuth(AuctionAuth)
  , ContractOutput
  , VerificationKey
  , mkContractOutput
  , vkeyCodec
  )
import HydraAuctionOffchain.Helpers (getInlineDatum)
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Wallet (SignMessageError, askWalletVk)

newtype DiscoverSellerSigContractParams = DiscoverSellerSigContractParams
  { auctionCs :: CurrencySymbol
  , sellerAddress :: Address
  , bidderVk :: Maybe VerificationKey
  }

derive instance Generic DiscoverSellerSigContractParams _
derive instance Newtype DiscoverSellerSigContractParams _
derive instance Eq DiscoverSellerSigContractParams

instance Show DiscoverSellerSigContractParams where
  show = genericShow

instance HasJson DiscoverSellerSigContractParams NetworkId where
  jsonCodec network = const (discoverSellerSigContractParamsCodec network)

discoverSellerSigContractParamsCodec
  :: NetworkId -> CA.JsonCodec DiscoverSellerSigContractParams
discoverSellerSigContractParamsCodec network =
  wrapIso DiscoverSellerSigContractParams $ CA.object "DiscoverSellerSigContractParams" $
    CAR.record
      { auctionCs: currencySymbolCodec
      , sellerAddress: addressCodec network
      , bidderVk: CA.maybe vkeyCodec
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
  bidderVk <-
    case params.bidderVk of
      Just vk -> pure vk
      Nothing ->
        _.vkey <$>
          withExceptT DiscoverSellerSig_Error_CouldNotGetBidderPubKey
            askWalletVk

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
    AuctionAuth auctionAuth <- getInlineDatum txOut
    snd <$>
      bool Nothing (Array.find (eq bidderVk <<< fst) auctionAuth.signatures)
        (auctionAuth.auctionCs == auctionCs)

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
  errorCodePrefix = const "DiscoverSellerSignature"
  errorMessage = case _ of
    DiscoverSellerSig_Error_CouldNotGetSellerPubKeyHash ->
      "Could not get seller pub key hash from address."

    DiscoverSellerSig_Error_CouldNotGetBidderPubKey err ->
      "Could not get bidder pub key, error: " <> show err <> "."
