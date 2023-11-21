module HydraAuctionOffchain.Contract.AnnounceAuction
  ( AnnounceAuctionContractError
      ( AnnounceAuctionInvalidAuctionTerms
      , AnnounceAuctionCouldNotGetWalletUtxos
      , AnnounceAuctionCouldNotCoverAuctionLot
      , AnnounceAuctionEmptyAuctionLotUtxoMap
      , AnnounceAuctionCurrentTimeAfterBiddingStart
      , AnnounceAuctionCouldNotGetAuctionCurrencySymbol
      )
  , AnnounceAuctionContractParams(AnnounceAuctionContractParams)
  , announceAuctionContract
  , mkAnnounceAuctionContractWithErrors
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId, scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (mintingPolicy, unspentOutputs) as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Time (to) as Time
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustMintValueWithRedeemer
  , mustPayToScript
  , mustSpendPubKeyOutput
  , mustValidateIn
  ) as Constraints
import Contract.Utxos (UtxoMap)
import Contract.Value (TokenName, Value, scriptCurrencySymbol)
import Contract.Value (singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionStrategy(SelectionStrategyMinimal)
  , performMultiAssetSelection
  )
import Ctl.Internal.CoinSelection.UtxoIndex (buildUtxoIndex)
import Ctl.Internal.Plutus.Conversion (fromPlutusUtxoMap, fromPlutusValue, toPlutusUtxoMap)
import Data.Array (head) as Array
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (hush)
import Data.Map (isEmpty, keys, toUnfoldable, union) as Map
import Data.Typelevel.Undefined (undefined)
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Codec (class HasJson)
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
  , AuctionTermsValidationError
  , ContractOutput
  , ContractResult
  , emptySubmitTxData
  , mkContractOutput
  , submitTxReturningContractResult
  , validateAuctionTerms
  )
import Partial.Unsafe (unsafePartial)

newtype AnnounceAuctionContractParams = AnnounceAuctionContractParams
  { auctionTerms :: AuctionTerms
  -- Allows the user to provide additional utxos to cover auction lot Value. This can be useful
  -- if some portion of the Value is, for example, locked at a multi-signature address.
  , additionalAuctionLotUtxos :: UtxoMap
  }

derive instance Generic AnnounceAuctionContractParams _
derive instance Newtype AnnounceAuctionContractParams _
derive instance Eq AnnounceAuctionContractParams

instance Show AnnounceAuctionContractParams where
  show = genericShow

instance HasJson AnnounceAuctionContractParams where
  jsonCodec = const announceAuctionContractParamsCodec

announceAuctionContractParamsCodec :: CA.JsonCodec AnnounceAuctionContractParams
announceAuctionContractParamsCodec = undefined

announceAuctionContract
  :: AnnounceAuctionContractParams
  -> Contract (ContractOutput TransactionHash)
announceAuctionContract =
  mkContractOutput _.txHash <<< mkAnnounceAuctionContractWithErrors

mkAnnounceAuctionContractWithErrors
  :: AnnounceAuctionContractParams
  -> ExceptT AnnounceAuctionContractError Contract ContractResult
mkAnnounceAuctionContractWithErrors (AnnounceAuctionContractParams params) = do
  -- Check auction terms:
  validateAuctionTerms params.auctionTerms #
    validation (throwError <<< AnnounceAuctionInvalidAuctionTerms) pure

  -- Select utxos to cover auction lot Value:
  let { auctionLot, biddingStart } = unwrap params.auctionTerms
  utxos <- getWalletUtxos !? AnnounceAuctionCouldNotGetWalletUtxos
  let utxos' = Map.union utxos params.additionalAuctionLotUtxos
  auctionLotUtxos <- selectUtxos utxos' auctionLot !? AnnounceAuctionCouldNotCoverAuctionLot
  when (Map.isEmpty auctionLotUtxos) $
    throwError AnnounceAuctionEmptyAuctionLotUtxoMap -- impossible

  -- Select nonce utxo from the auction lot utxos:
  let nonceUtxo = unsafePartial fromJust $ Array.head $ Map.toUnfoldable auctionLotUtxos
  let nonceOref = fst nonceUtxo

  -- Check that the current time < bidding start time:
  nowTime <- lift currentTime
  unless (nowTime < biddingStart) $
    throwError AnnounceAuctionCurrentTimeAfterBiddingStart

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

selectUtxos :: UtxoMap -> Value -> Contract (Maybe UtxoMap)
selectUtxos utxos requiredValue = do
  networkId <- getNetworkId
  let utxoIndex = buildUtxoIndex $ fromPlutusUtxoMap networkId utxos
  hush <$> runExceptT do
    selState <- performMultiAssetSelection SelectionStrategyMinimal utxoIndex
      (fromPlutusValue requiredValue)
    let selectedUtxos = (unwrap selState).selectedUtxos
    pure $ unsafePartial fromJust $ toPlutusUtxoMap selectedUtxos

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data AnnounceAuctionContractError
  = AnnounceAuctionInvalidAuctionTerms (Array AuctionTermsValidationError)
  | AnnounceAuctionCouldNotGetWalletUtxos
  | AnnounceAuctionCouldNotCoverAuctionLot
  | AnnounceAuctionEmptyAuctionLotUtxoMap
  | AnnounceAuctionCurrentTimeAfterBiddingStart
  | AnnounceAuctionCouldNotGetAuctionCurrencySymbol

derive instance Generic AnnounceAuctionContractError _
derive instance Eq AnnounceAuctionContractError

instance Show AnnounceAuctionContractError where
  show = genericShow

instance ToContractError AnnounceAuctionContractError where
  toContractError = wrap <<< case _ of
    AnnounceAuctionInvalidAuctionTerms validationErrors ->
      { errorCode: "AnnounceAuction01"
      , message: "Invalid auction terms, errors: " <> show validationErrors <> "."
      }
    AnnounceAuctionCouldNotGetWalletUtxos ->
      { errorCode: "AnnounceAuction02"
      , message: "Could not get wallet utxos."
      }
    AnnounceAuctionCouldNotCoverAuctionLot ->
      { errorCode: "AnnounceAuction03"
      , message: "Could not cover auction lot Value."
      }
    AnnounceAuctionEmptyAuctionLotUtxoMap ->
      { errorCode: "AnnounceAuction04"
      , message: "Impossible: Auction lot utxo map cannot be empty."
      }
    AnnounceAuctionCurrentTimeAfterBiddingStart ->
      { errorCode: "AnnounceAuction05"
      , message: "Tx cannot be submitted after bidding start time."
      }
    AnnounceAuctionCouldNotGetAuctionCurrencySymbol ->
      { errorCode: "AnnounceAuction06"
      , message: "Could not get Auction currency symbol from minting policy."
      }
