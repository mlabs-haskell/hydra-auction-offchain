module HydraAuctionOffchain.Contract.AnnounceAuction
  ( AnnounceAuctionContractError
      ( AnnounceAuction_Error_InvalidAuctionTerms
      , AnnounceAuction_Error_CouldNotGetWalletUtxos
      , AnnounceAuction_Error_CouldNotGetAdditionalAuctionLotUtxos
      , AnnounceAuction_Error_CouldNotCoverAuctionLot
      , AnnounceAuction_Error_EmptyAuctionLotUtxoMap
      , AnnounceAuction_Error_CurrentTimeAfterBiddingStart
      , AnnounceAuction_Error_CouldNotBuildAuctionValidators
      , AnnounceAuction_Error_CouldNotGetOwnPubKey
      )
  , AnnounceAuctionContractOutput(AnnounceAuctionContractOutput)
  , AnnounceAuctionContractParams(AnnounceAuctionContractParams)
  , AnnounceAuctionContractResult
  , announceAuctionContract
  , mkAnnounceAuctionContractWithErrors
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId, scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Config (NetworkId)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (mintingPolicy, unspentOutputs) as Lookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Time (POSIXTimeRange, to)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustMintCurrencyUsingNativeScript
  , mustMintValueWithRedeemer
  , mustPayToScript
  , mustPayToScriptWithScriptRef
  , mustSpendPubKeyOutput
  , mustValidateIn
  ) as Constraints
import Contract.Utxos (UtxoMap, getUtxo)
import Contract.Value (TokenName, Value, scriptCurrencySymbol)
import Contract.Value (singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Control.Error.Util ((!?))
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionStrategy(SelectionStrategyMinimal)
  , performMultiAssetSelection
  )
import Ctl.Internal.CoinSelection.UtxoIndex (buildUtxoIndex)
import Ctl.Internal.Plutus.Conversion (fromPlutusUtxoMap, fromPlutusValue, toPlutusUtxoMap)
import Data.Array (head) as Array
import Data.Codec.Argonaut (JsonCodec, array, object) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (hush)
import Data.Map (fromFoldable, isEmpty, keys, toUnfoldable, union) as Map
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Codec (orefCodec, transactionHashCodec)
import HydraAuctionOffchain.Contract.MintingPolicies
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , mkAuctionMintingPolicy
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.PersonalOracle (PersonalOracle, mkPersonalOracle)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , ActorRole(ActorRoleSeller)
  , AuctionActor(AuctionActor)
  , AuctionEscrowState(AuctionAnnounced)
  , AuctionInfo
  , AuctionInfoExtended
  , AuctionPolicyRedeemer(MintAuction)
  , AuctionTermsInput
  , AuctionTermsValidationError
  , ContractOutput
  , ContractResult'
  , DelegateInfo
  , assetToValue
  , auctionInfoExtendedCodec
  , auctionTermsInputCodec
  , delegateInfoCodec
  , emptySubmitTxData
  , mkAuctionInfoExtended
  , mkAuctionTerms
  , mkContractOutput
  , submitTxReturningContractResult
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators
  ( MkAuctionValidatorsError
  , mkAuctionMetadataValidator
  , mkAuctionValidators
  )
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Wallet (SignMessageError, askWalletVk)
import JS.BigInt (fromInt) as BigInt
import Partial.Unsafe (unsafePartial)
import Record (merge) as Record

newtype AnnounceAuctionContractParams = AnnounceAuctionContractParams
  { auctionTerms :: AuctionTermsInput
  , delegateInfo :: Maybe DelegateInfo
  -- Allows the user to provide additional utxos to cover auction lot Value. This can be useful
  -- if some portion of the Value is, for example, locked at a multi-signature address.
  , additionalAuctionLotOrefs :: Array TransactionInput
  }

derive instance Generic AnnounceAuctionContractParams _
derive instance Newtype AnnounceAuctionContractParams _
derive instance Eq AnnounceAuctionContractParams

instance Show AnnounceAuctionContractParams where
  show = genericShow

instance HasJson AnnounceAuctionContractParams anyParams where
  jsonCodec _ = const announceAuctionContractParamsCodec

announceAuctionContractParamsCodec :: CA.JsonCodec AnnounceAuctionContractParams
announceAuctionContractParamsCodec =
  wrapIso AnnounceAuctionContractParams $ CA.object "AnnounceAuctionContractParams" $
    CAR.record
      { auctionTerms: auctionTermsInputCodec
      , delegateInfo: CA.maybe delegateInfoCodec
      , additionalAuctionLotOrefs: CA.array orefCodec
      }

newtype AnnounceAuctionContractOutput = AnnounceAuctionContractOutput
  { txHash :: TransactionHash
  , auctionInfo :: AuctionInfoExtended
  }

derive instance Generic AnnounceAuctionContractOutput _

derive instance Newtype AnnounceAuctionContractOutput _
derive instance Eq AnnounceAuctionContractOutput

instance Show AnnounceAuctionContractOutput where
  show = genericShow

instance HasJson AnnounceAuctionContractOutput NetworkId where
  jsonCodec network = const (announceAuctionContractOutputCodec network)

type AnnounceAuctionContractResult = ContractResult' (auctionInfo :: AuctionInfoExtended)

announceAuctionContractOutputCodec :: NetworkId -> CA.JsonCodec AnnounceAuctionContractOutput
announceAuctionContractOutputCodec network =
  wrapIso AnnounceAuctionContractOutput $ CA.object "AnnounceAuctionContractOutput" $
    CAR.record
      { txHash: transactionHashCodec
      , auctionInfo: auctionInfoExtendedCodec network
      }

announceAuctionContract
  :: AnnounceAuctionContractParams
  -> Contract (ContractOutput AnnounceAuctionContractOutput)
announceAuctionContract =
  mkContractOutput resultToOutput <<< mkAnnounceAuctionContractWithErrors
  where
  resultToOutput :: AnnounceAuctionContractResult -> AnnounceAuctionContractOutput
  resultToOutput rec = wrap
    { txHash: rec.txHash
    , auctionInfo: rec.auctionInfo
    }

mkAnnounceAuctionContractWithErrors
  :: AnnounceAuctionContractParams
  -> ExceptT AnnounceAuctionContractError Contract AnnounceAuctionContractResult
mkAnnounceAuctionContractWithErrors (AnnounceAuctionContractParams params) = do
  -- Get pkh and vkey, build AuctionTerms:
  { vkey, address, pkh: sellerPkh } <-
    withExceptT AnnounceAuction_Error_CouldNotGetOwnPubKey
      askWalletVk

  -- Check auction terms:
  let auctionTerms = mkAuctionTerms params.auctionTerms address vkey
  validateAuctionTerms auctionTerms #
    validation (throwError <<< AnnounceAuction_Error_InvalidAuctionTerms) pure

  -- Select utxos to cover auction lot Value:
  let { auctionLot, biddingStart } = unwrap auctionTerms
  utxos <- getWalletUtxos !? AnnounceAuction_Error_CouldNotGetWalletUtxos
  additionalAuctionLotUtxos <- queryUtxos params.additionalAuctionLotOrefs
    !? AnnounceAuction_Error_CouldNotGetAdditionalAuctionLotUtxos
  let utxos' = Map.union utxos additionalAuctionLotUtxos
  auctionLotUtxos <- selectUtxos utxos' auctionLot
    !? AnnounceAuction_Error_CouldNotCoverAuctionLot
  when (Map.isEmpty auctionLotUtxos) $
    throwError AnnounceAuction_Error_EmptyAuctionLotUtxoMap -- impossible

  -- Select nonce utxo from the auction lot utxos:
  let nonceUtxo = unsafePartial fromJust $ Array.head $ Map.toUnfoldable auctionLotUtxos
  let nonceOref = fst nonceUtxo

  -- Check that the current time < bidding start time:
  nowTime <- lift currentTime
  unless (nowTime < biddingStart) $
    throwError AnnounceAuction_Error_CurrentTimeAfterBiddingStart

  -- Get auction minting policy:
  auctionMetadataSh <- lift $ mkAuctionMetadataValidator <#> unwrap <<< validatorHash
  auctionMintingPolicy <- lift $ mkAuctionMintingPolicy auctionMetadataSh nonceOref
  let auctionCs = scriptCurrencySymbol auctionMintingPolicy

  -- Get auction validators:
  validators <-
    withExceptT AnnounceAuction_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms
  let validatorHashes = validatorHash <$> validators
  let validatorAddresses = unwrap $ flip scriptHashAddress Nothing <$> validatorHashes

  -- Get auction metadata validator hash:
  metadataValidatorHash <- lift $ validatorHash <$> mkAuctionMetadataValidator
  let
    mkAuctionToken :: TokenName -> Value
    mkAuctionToken tokenName = Value.singleton auctionCs tokenName one

    auctionTokenBundle :: Value
    auctionTokenBundle = foldMap mkAuctionToken
      [ auctionEscrowTokenName
      , auctionMetadataTokenName
      , standingBidTokenName
      ]

    mintAuctionRedeemer :: Redeemer
    mintAuctionRedeemer = wrap $ toData MintAuction

    -- AuctionEscrow -------------------------------------------------

    auctionEscrowValidatorHash :: ValidatorHash
    auctionEscrowValidatorHash = (unwrap validatorHashes).auctionEscrow

    auctionEscrowValue :: Value
    auctionEscrowValue = auctionLot <> mkAuctionToken auctionEscrowTokenName
      <> mkAuctionToken standingBidTokenName

    auctionEscrowDatum :: Datum
    auctionEscrowDatum = wrap $ toData AuctionAnnounced

    -- AuctionInfo ---------------------------------------------------

    auctionInfoValue :: Value
    auctionInfoValue = mkAuctionToken auctionMetadataTokenName

    auctionInfo :: AuctionInfo
    auctionInfo = wrap
      { auctionId: auctionCs
      , auctionTerms
      , delegateInfo: params.delegateInfo
      , auctionEscrowAddr: validatorAddresses.auctionEscrow
      , bidderDepositAddr: validatorAddresses.bidderDeposit
      , feeEscrowAddr: validatorAddresses.feeEscrow
      , standingBidAddr: validatorAddresses.standingBid
      }

    auctionInfoDatum :: Datum
    auctionInfoDatum = wrap $ toData auctionInfo

    -- AuctionActor --------------------------------------------------

    sellerOracle :: PersonalOracle
    sellerOracle = mkPersonalOracle $ wrap sellerPkh

    sellerOracleTokenValue :: Value
    sellerOracleTokenValue = assetToValue sellerOracle.assetClass one

    auctionActorDatum :: Datum
    auctionActorDatum = wrap $ toData $ AuctionActor
      { auctionInfo: mkAuctionInfoExtended auctionInfo Nothing
      , role: ActorRoleSeller
      }

    --

    txValidRange :: POSIXTimeRange
    txValidRange = to $ biddingStart - wrap (BigInt.fromInt 1000)

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Spend auction lot utxos, including nonce utxo:
        foldMap Constraints.mustSpendPubKeyOutput $ Map.keys auctionLotUtxos

      -- Mint auction state, auction metadata, and standing bid tokens:
      , Constraints.mustMintValueWithRedeemer mintAuctionRedeemer auctionTokenBundle

      -- Lock auction lot with auction state and standing bid tokens
      -- at the auction escrow validator address, set state to 
      -- AuctionAnnounced:
      , Constraints.mustPayToScript auctionEscrowValidatorHash auctionEscrowDatum DatumInline
          auctionEscrowValue

      -- Mint seller's personal oracle token:
      , Constraints.mustMintCurrencyUsingNativeScript sellerOracle.nativeScript
          (unwrap sellerOracle.assetClass).tokenName
          one

      -- Lock auction actor datum with personal oracle token at 
      -- the seller's personal oracle address:
      , Constraints.mustPayToScript (wrap $ sellerOracle.nativeScriptHash) auctionActorDatum
          DatumInline
          sellerOracleTokenValue

      -- Lock auction metadata datum and auction metadata token 
      -- at the auction metadata validator address:
      , Constraints.mustPayToScriptWithScriptRef metadataValidatorHash auctionInfoDatum
          DatumInline
          -- Attach standing bid reference script to meet the max tx
          -- size requirement for subsequent transactions.
          (PlutusScriptRef $ unwrap (unwrap validators).standingBid)
          auctionInfoValue

      -- Set transaction validity interval to registration period:
      , Constraints.mustValidateIn txValidRange
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs auctionLotUtxos
      , Lookups.mintingPolicy auctionMintingPolicy
      ]

  result <- lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }
  pure $ Record.merge result
    { auctionInfo:
        mkAuctionInfoExtended auctionInfo $ Just $ wrap
          { transactionId: result.txHash
          , index: zero
          }
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

queryUtxos :: Array TransactionInput -> Contract (Maybe UtxoMap)
queryUtxos = map (map Map.fromFoldable <<< sequence) <<< parTraverse getUtxo'
  where
  getUtxo'
    :: TransactionInput
    -> Contract (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
  getUtxo' oref =
    getUtxo oref <#>
      map (\output -> oref /\ wrap { output, scriptRef: Nothing })

----------------------------------------------------------------------
-- Errors

data AnnounceAuctionContractError
  = AnnounceAuction_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | AnnounceAuction_Error_CouldNotGetWalletUtxos
  | AnnounceAuction_Error_CouldNotGetAdditionalAuctionLotUtxos
  | AnnounceAuction_Error_CouldNotCoverAuctionLot
  | AnnounceAuction_Error_EmptyAuctionLotUtxoMap
  | AnnounceAuction_Error_CurrentTimeAfterBiddingStart
  | AnnounceAuction_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | AnnounceAuction_Error_CouldNotGetOwnPubKey SignMessageError

derive instance Generic AnnounceAuctionContractError _
derive instance Eq AnnounceAuctionContractError

instance Show AnnounceAuctionContractError where
  show = genericShow

instance ToContractError AnnounceAuctionContractError where
  errorCodePrefix = const "AnnounceAuction"
  errorMessage = case _ of
    AnnounceAuction_Error_InvalidAuctionTerms validationErrors ->
      "Invalid auction terms, errors: " <> show validationErrors <> "."

    AnnounceAuction_Error_CouldNotGetWalletUtxos ->
      "Could not get wallet utxos."

    AnnounceAuction_Error_CouldNotGetAdditionalAuctionLotUtxos ->
      "Could not resolve provided action lot output references."

    AnnounceAuction_Error_CouldNotCoverAuctionLot ->
      "Could not cover auction lot Value."

    AnnounceAuction_Error_EmptyAuctionLotUtxoMap ->
      "Impossible: Auction lot utxo map cannot be empty."

    AnnounceAuction_Error_CurrentTimeAfterBiddingStart ->
      "Tx cannot be submitted after bidding start time."

    AnnounceAuction_Error_CouldNotBuildAuctionValidators err ->
      "Could not build auction validators, error: " <> show err <> "."

    AnnounceAuction_Error_CouldNotGetOwnPubKey err ->
      "Could not get own public key, error: " <> show err <> "."
