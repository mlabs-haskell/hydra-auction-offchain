module HydraAuctionOffchain.Contract.AnnounceAuction where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints
  ( mustMintValueWithRedeemer
  , mustPayToScript
  , mustSpendPubKeyOutput
  ) as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value, scriptCurrencySymbol)
import Contract.Value (singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (head) as Array
import Data.Map (toUnfoldable) as Map
import Data.Typelevel.Undefined (undefined)
import HydraAuctionOffchain.Contract.Scripts.AuctionMintingPolicy
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , mkAuctionMintingPolicy
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionTerms(AuctionTerms)
  , ContractResult
  , emptySubmitTxData
  , submitTxReturningContractResult
  )

mkAnnounceAuctionContractWithErrors
  :: AuctionTerms
  -> ExceptT AnnounceAuctionContractError Contract ContractResult
mkAnnounceAuctionContractWithErrors auctionTerms@(AuctionTerms auctionTermsRec) = do
  utxos <- getWalletUtxos !? AnnounceAuctionCouldNotGetWalletUtxos
  -- TODO: impl an algorithm for selecting optimal nonce utxo based on size / token bundle
  nonceUtxo <- fst <$> Array.head (Map.toUnfoldable utxos :: Array _) ??
    AnnounceAuctionCouldNotGetNonceUtxo
  auctionMintingPolicy <- lift $ mkAuctionMintingPolicy nonceUtxo
  let
    auctionCurrencySymbol :: CurrencySymbol
    auctionCurrencySymbol = scriptCurrencySymbol auctionMintingPolicy

    mkAuctionToken :: TokenName -> Value
    mkAuctionToken tokenName = Value.singleton auctionCurrencySymbol tokenName one

    auctionTokenBundle :: Value
    auctionTokenBundle = foldMap mkAuctionToken
      [ auctionEscrowTokenName
      , auctionMetadataTokenName
      , standingBidTokenName
      ]

    escrowValue :: Value
    escrowValue =
      auctionTermsRec.auctionLot
        <> mkAuctionToken auctionEscrowTokenName
        <> mkAuctionToken standingBidTokenName

    mintAuctionRedeemer :: Redeemer
    mintAuctionRedeemer =

    constraints :: TxConstraints Void Void
    constraints = mconcat
      [ Constraints.mustSpendPubKeyOutput nonceUtxo
      , Constraints.mustMintValueWithRedeemer mintAuctionRedeemer auctionTokenBundle
      , Constraints.mustPayToScript escrowValidatorHash escrowStateDatum DatumInline
          escrowValue
      ]

    lookups :: ScriptLookups Void
    lookups = mempty

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data AnnounceAuctionContractError
  = AnnounceAuctionCouldNotGetWalletUtxos
  | AnnounceAuctionCouldNotGetNonceUtxo

derive instance Generic AnnounceAuctionContractError _
derive instance Eq AnnounceAuctionContractError

instance Show AnnounceAuctionContractError where
  show = genericShow

instance ToContractError AnnounceAuctionContractError where
  toContractError = case _ of
    AnnounceAuctionCouldNotGetWalletUtxos ->
      { errorCode: "AnnounceAuction01"
      , message: "Could not get wallet utxos."
      }
    AnnounceAuctionCouldNotGetNonceUtxo ->
      { errorCode: "AnnounceAuction02"
      , message: "Could not get nonce utxo to parameterize Auction minting policy."
      }
