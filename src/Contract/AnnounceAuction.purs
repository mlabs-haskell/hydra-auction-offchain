module HydraAuctionOffchain.Contract.AnnounceAuction where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (Datum, Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (mintingPolicy, unspentOutputs, validator) as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustMintValueWithRedeemer
  , mustPayToScript
  , mustSpendPubKeyOutput
  ) as Constraints
import Contract.Utxos (UtxoMap)
import Contract.Value (TokenName, Value, scriptCurrencySymbol)
import Contract.Value (geq, singleton) as Value
import Control.Error.Util ((??))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Data.Array (head) as Array
import Data.Lens (view)
import Data.Map (isEmpty, keys, toUnfoldable, values) as Map
import HydraAuctionOffchain.Contract.Scripts
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , mkAuctionEscrowValidator
  , mkAuctionMintingPolicy
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionEscrowState(AuctionAnnounced)
  , AuctionPolicyRedeemer(MintAuction)
  , AuctionTerms
  , ContractResult
  , emptySubmitTxData
  , submitTxReturningContractResult
  )
import Partial.Unsafe (unsafePartial)

type AnnounceAuctionContractParams =
  { auctionTerms :: AuctionTerms
  , auctionLotUtxos :: UtxoMap
  }

mkAnnounceAuctionContractWithErrors
  :: AnnounceAuctionContractParams
  -> ExceptT AnnounceAuctionContractError Contract ContractResult
mkAnnounceAuctionContractWithErrors params = do
  -- Check that the auction lot utxo map is not empty:
  let auctionLotUtxos = params.auctionLotUtxos
  when (Map.isEmpty auctionLotUtxos) $
    throwError AnnounceAuctionEmptyAuctionLotUtxoMap

  -- Check that the auction lot utxo map covers Value specified in AuctionTerms:
  let auctionLotValue = (unwrap params.auctionTerms).auctionLot
  let auctionLotCoverValue = foldMap (view (_output <<< _amount)) $ Map.values auctionLotUtxos
  unless (auctionLotCoverValue `Value.geq` auctionLotValue) $
    throwError AnnounceAuctionCouldNotCoverAuctionLot

  -- Select nonce utxo from the provided auction lot utxo map:
  let nonceUtxo = unsafePartial fromJust $ Array.head $ Map.toUnfoldable auctionLotUtxos
  let nonceOref = fst nonceUtxo

  -- Auction minting policy:
  auctionMintingPolicy <- lift $ mkAuctionMintingPolicy nonceOref
  auctionCurrencySymbol <- scriptCurrencySymbol auctionMintingPolicy ??
    AnnounceAuctionCouldNotGetAuctionCurrencySymbol

  -- Auction escrow validator:
  escrowValidator <- lift $ mkAuctionEscrowValidator params.auctionTerms
  let
    mkAuctionToken :: TokenName -> Value
    mkAuctionToken tokenName = Value.singleton auctionCurrencySymbol tokenName one

    auctionTokenBundle :: Value
    auctionTokenBundle = foldMap mkAuctionToken
      [ auctionEscrowTokenName
      , auctionMetadataTokenName
      , standingBidTokenName
      ]

    mintAuctionRedeemer :: Redeemer
    mintAuctionRedeemer = wrap $ toData MintAuction

    escrowValidatorHash :: ValidatorHash
    escrowValidatorHash = validatorHash escrowValidator

    escrowValue :: Value
    escrowValue =
      auctionLotValue <> mkAuctionToken auctionEscrowTokenName
        <> mkAuctionToken standingBidTokenName

    escrowStateDatum :: Datum
    escrowStateDatum = wrap $ toData AuctionAnnounced

    constraints :: TxConstraints Void Void
    constraints = mconcat
      [ -- Spend auction lot utxos, including nonce utxo:
        foldMap Constraints.mustSpendPubKeyOutput $ Map.keys auctionLotUtxos

      -- Mint auction state, auction metadata, and standing bid tokens:
      , Constraints.mustMintValueWithRedeemer mintAuctionRedeemer auctionTokenBundle

      -- Lock auction lot with auction state and standing bid tokens at the auction escrow
      -- validator address, set state to AuctionAnnounced:
      , Constraints.mustPayToScript escrowValidatorHash escrowStateDatum DatumInline
          escrowValue
      ]

    lookups :: ScriptLookups Void
    lookups = mconcat
      [ Lookups.unspentOutputs auctionLotUtxos
      , Lookups.mintingPolicy auctionMintingPolicy
      , Lookups.validator escrowValidator
      ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data AnnounceAuctionContractError
  = AnnounceAuctionEmptyAuctionLotUtxoMap
  | AnnounceAuctionCouldNotCoverAuctionLot
  | AnnounceAuctionCouldNotGetAuctionCurrencySymbol

derive instance Generic AnnounceAuctionContractError _
derive instance Eq AnnounceAuctionContractError

instance Show AnnounceAuctionContractError where
  show = genericShow

instance ToContractError AnnounceAuctionContractError where
  toContractError = case _ of
    AnnounceAuctionEmptyAuctionLotUtxoMap ->
      { errorCode: "AnnounceAuction01"
      , message: "Auction lot utxo map cannot be empty."
      }
    AnnounceAuctionCouldNotCoverAuctionLot ->
      { errorCode: "AnnounceAuction02"
      , message: "Could not cover auction lot Value with provided utxo map."
      }
    AnnounceAuctionCouldNotGetAuctionCurrencySymbol ->
      { errorCode: "AnnounceAuction03"
      , message: "Could not get Auction currency symbol from minting policy."
      }
