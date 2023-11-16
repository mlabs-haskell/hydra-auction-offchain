module HydraAuctionOffchain.Contract.AnnounceAuction where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (mintingPolicy, unspentOutputs) as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Time (to) as Time
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustMintValueWithRedeemer
  , mustPayToScript
  , mustSpendPubKeyOutput
  , mustValidateIn
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
  , mkAuctionMetadataValidator
  , mkAuctionMintingPolicy
  , mkAuctionValidators
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionEscrowState(AuctionAnnounced)
  , AuctionInfo(AuctionInfo)
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
  let { auctionLot, biddingStart } = unwrap params.auctionTerms
  let auctionLotCoverValue = foldMap (view (_output <<< _amount)) $ Map.values auctionLotUtxos
  unless (auctionLotCoverValue `Value.geq` auctionLot) $
    throwError AnnounceAuctionCouldNotCoverAuctionLot

  -- Check that the current time < bidding start time:
  nowTime <- lift currentTime
  unless (nowTime < biddingStart) $
    throwError AnnounceAuctionCurrentTimeAfterBiddingStart

  -- Select nonce utxo from the provided auction lot utxo map:
  let nonceUtxo = unsafePartial fromJust $ Array.head $ Map.toUnfoldable auctionLotUtxos
  let nonceOref = fst nonceUtxo

  -- Get auction minting policy:
  auctionMintingPolicy <- lift $ mkAuctionMintingPolicy nonceOref
  auctionCurrencySymbol <- scriptCurrencySymbol auctionMintingPolicy ??
    AnnounceAuctionCouldNotGetAuctionCurrencySymbol

  -- Get auction validators:
  validatorHashes <- lift $ map validatorHash <$> mkAuctionValidators params.auctionTerms
  let validatorAddresses = unwrap $ flip scriptHashAddress Nothing <$> validatorHashes

  -- Get auction metadata validator hash:
  metadataValidatorHash <- lift $ validatorHash <$> mkAuctionMetadataValidator
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

    auctionEscrowValidatorHash :: ValidatorHash
    auctionEscrowValidatorHash = (unwrap validatorHashes).auctionEscrow

    auctionEscrowValue :: Value
    auctionEscrowValue = auctionLot <> mkAuctionToken auctionEscrowTokenName
      <> mkAuctionToken standingBidTokenName

    auctionEscrowDatum :: Datum
    auctionEscrowDatum = wrap $ toData AuctionAnnounced

    auctionInfoValue :: Value
    auctionInfoValue = mkAuctionToken auctionMetadataTokenName

    auctionInfoDatum :: Datum
    auctionInfoDatum = wrap $ toData $ AuctionInfo
      { auctionId: auctionCurrencySymbol
      , auctionTerms: params.auctionTerms
      , auctionEscrowAddr: validatorAddresses.auctionEscrow
      , bidderDepositAddr: validatorAddresses.bidderDeposit
      , feeEscrowAddr: validatorAddresses.feeEscrow
      , standingBidAddr: validatorAddresses.standingBid
      }

    constraints :: TxConstraints Void Void
    constraints = mconcat
      [ -- Spend auction lot utxos, including nonce utxo:
        foldMap Constraints.mustSpendPubKeyOutput $ Map.keys auctionLotUtxos

      -- Mint auction state, auction metadata, and standing bid tokens:
      , Constraints.mustMintValueWithRedeemer mintAuctionRedeemer auctionTokenBundle

      -- Lock auction lot with auction state and standing bid tokens at the auction escrow
      -- validator address, set state to AuctionAnnounced:
      , Constraints.mustPayToScript auctionEscrowValidatorHash auctionEscrowDatum DatumInline
          auctionEscrowValue

      -- Lock auction metadata datum and auction metadata token at the auction metadata
      -- validator address:
      , Constraints.mustPayToScript metadataValidatorHash auctionInfoDatum DatumInline
          auctionInfoValue

      -- Tx must be included in the block before bidding starts:
      , Constraints.mustValidateIn $ Time.to biddingStart
      ]

    lookups :: ScriptLookups Void
    lookups = mconcat
      [ Lookups.unspentOutputs auctionLotUtxos
      , Lookups.mintingPolicy auctionMintingPolicy
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
  | AnnounceAuctionCurrentTimeAfterBiddingStart
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
    AnnounceAuctionCurrentTimeAfterBiddingStart ->
      { errorCode: "AnnounceAuction03"
      , message: "Tx cannot be submitted after bidding start time."
      }
    AnnounceAuctionCouldNotGetAuctionCurrencySymbol ->
      { errorCode: "AnnounceAuction04"
      , message: "Could not get Auction currency symbol from minting policy."
      }
