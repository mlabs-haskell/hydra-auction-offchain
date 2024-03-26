module HydraAuctionOffchain.Contract.AuthorizeBidders
  ( AuthBiddersContractError
      ( AuthBidders_Error_NoBiddersToAuthorize
      , AuthBidders_Error_CouldNotGetOwnPubKeyHash
      , AuthBidders_Error_CouldNotSignSellerMessage
      )
  , AuthBiddersContractParams(AuthBiddersContractParams)
  , authorizeBiddersContract
  , mkAuthorizeBiddersContractWithErrors
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (Datum, toData)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints (mustMintCurrencyUsingNativeScript, mustPayToScript) as Constraints
import Contract.Value (CurrencySymbol, Value)
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Error.Util ((!?))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (nub, null) as Array
import Data.Codec.Argonaut (JsonCodec, array, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Profunctor (wrapIso)
import HydraAuctionOffchain.Codec (class HasJson, currencySymbolCodec)
import HydraAuctionOffchain.Contract.PersonalOracle (PersonalOracle, mkPersonalOracle)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionAuth(AuctionAuth)
  , ContractOutput
  , ContractResult
  , VerificationKey
  , assetToValue
  , emptySubmitTxData
  , mkContractOutput
  , sellerSignatureMessage
  , submitTxReturningContractResult
  , vkeyBytes
  , vkeyCodec
  )
import HydraAuctionOffchain.Wallet (SignMessageError, signMessage)

newtype AuthBiddersContractParams = AuthBiddersContractParams
  { auctionCs :: CurrencySymbol
  , biddersToAuthorize :: Array VerificationKey
  }

derive instance Generic AuthBiddersContractParams _
derive instance Newtype AuthBiddersContractParams _
derive instance Eq AuthBiddersContractParams

instance Show AuthBiddersContractParams where
  show = genericShow

instance HasJson AuthBiddersContractParams where
  jsonCodec = const authBiddersContractParamsCodec

authBiddersContractParamsCodec :: CA.JsonCodec AuthBiddersContractParams
authBiddersContractParamsCodec =
  wrapIso AuthBiddersContractParams $ CA.object "AuthBiddersContractParams" $
    CAR.record
      { auctionCs: currencySymbolCodec
      , biddersToAuthorize: CA.array vkeyCodec
      }

authorizeBiddersContract
  :: AuthBiddersContractParams
  -> Contract (ContractOutput TransactionHash)
authorizeBiddersContract =
  mkContractOutput _.txHash <<< mkAuthorizeBiddersContractWithErrors

mkAuthorizeBiddersContractWithErrors
  :: AuthBiddersContractParams
  -> ExceptT AuthBiddersContractError Contract ContractResult
mkAuthorizeBiddersContractWithErrors (AuthBiddersContractParams params) = do
  let
    auctionCs = params.auctionCs
    biddersToAuthorize = Array.nub params.biddersToAuthorize

  -- Check that there is at least one bidder to authorize:
  when (Array.null biddersToAuthorize) $
    throwError AuthBidders_Error_NoBiddersToAuthorize

  -- Get own pkh:
  sellerPkh <- ownPaymentPubKeyHash !? AuthBidders_Error_CouldNotGetOwnPubKeyHash

  -- Generate signatures: 
  signatures <-
    for biddersToAuthorize $ \bidderVk -> do
      let payload = sellerSignatureMessage auctionCs $ vkeyBytes bidderVk
      { signature } <- withExceptT (AuthBidders_Error_CouldNotSignSellerMessage bidderVk) $
        signMessage payload
      pure $ bidderVk /\ signature

  let
    sellerOracle :: PersonalOracle
    sellerOracle = mkPersonalOracle sellerPkh

    sellerOracleTokenValue :: Value
    sellerOracleTokenValue = assetToValue sellerOracle.assetClass one

    auctionAuthDatum :: Datum
    auctionAuthDatum = wrap $ toData $ AuctionAuth { auctionCs, signatures }

    constraints :: TxConstraints
    constraints = mconcat
      [ Constraints.mustPayToScript (wrap $ sellerOracle.nativeScriptHash) auctionAuthDatum
          DatumInline
          sellerOracleTokenValue

      , Constraints.mustMintCurrencyUsingNativeScript sellerOracle.nativeScript
          (unwrap sellerOracle.assetClass).tokenName
          one
      ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { constraints = constraints
    }

----------------------------------------------------------------------
-- Errors

data AuthBiddersContractError
  = AuthBidders_Error_NoBiddersToAuthorize
  | AuthBidders_Error_CouldNotGetOwnPubKeyHash
  | AuthBidders_Error_CouldNotSignSellerMessage VerificationKey SignMessageError

derive instance Generic AuthBiddersContractError _
derive instance Eq AuthBiddersContractError

instance Show AuthBiddersContractError where
  show = genericShow

instance ToContractError AuthBiddersContractError where
  toContractError = wrap <<< case _ of
    AuthBidders_Error_NoBiddersToAuthorize ->
      { errorCode: "AuthorizeBidders01"
      , message: "There must be at least one bidder to authorize."
      }
    AuthBidders_Error_CouldNotGetOwnPubKeyHash ->
      { errorCode: "AuthorizeBidders02"
      , message: "Could not get own public key hash."
      }
    AuthBidders_Error_CouldNotSignSellerMessage bidderVk err ->
      { errorCode: "AuthorizeBidders03"
      , message:
          "Could not generate signature for bidder with " <> show bidderVk <> ", error: "
            <> show err
            <> "."
      }
