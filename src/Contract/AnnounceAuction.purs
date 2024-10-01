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
      , AnnounceAuction_Error_SellerAddressConversionFailure
      , AnnounceAuction_Error_AuctionLotValueConversionFailure
      , AnnounceAuction_Error_GetAvailableSlotsRequestFailed
      , AnnounceAuction_Error_NoAvailableDelegateGroupSlots
      , AnnounceAuction_Error_ReserveSlotRequestFailed
      , AnnounceAuction_Error_CouldNotReserveDelegateGroupSlot
      , AnnounceAuction_Error_HostAuctionRequestFailed
      , AnnounceAuction_Error_CouldNotHostAuction
      )
  , AnnounceAuctionContractOutput(AnnounceAuctionContractOutput)
  , AnnounceAuctionContractParams(AnnounceAuctionContractParams)
  , AnnounceAuctionContractResult
  , announceAuctionContract
  , mkAnnounceAuctionContractWithErrors
  ) where

import Contract.Prelude hiding (foldM)

import Cardano.Plutus.Types.Address (fromCardano) as Plutus.Address
import Cardano.Plutus.Types.Address (scriptHashAddress)
import Cardano.Plutus.Types.Value (toCardano) as Plutus.Value
import Cardano.Types
  ( AssetName
  , Ed25519KeyHash
  , NetworkId
  , PlutusData
  , RedeemerDatum
  , ScriptHash
  , ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , Value
  )
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.Int (one) as Cardano.Int
import Cardano.Types.Mint (fromMultiAsset) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Address (getNetworkId)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (plutusMintingPolicy, unspentOutputs) as Lookups
import Contract.Time (POSIXTimeRange, to)
import Contract.Transaction (awaitTxConfirmed)
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
import Contract.Value (getMultiAsset, singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse)
import Control.Safely (foldM)
import Ctl.Internal.BalanceTx.CoinSelection
  ( SelectionStrategy(SelectionStrategyMinimal)
  , performMultiAssetSelection
  )
import Ctl.Internal.CoinSelection.UtxoIndex (buildUtxoIndex)
import Ctl.Internal.Types.Val (fromValue) as Val
import Data.Array (cons, head, uncons) as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, array, object) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (hush)
import Data.List (fromFoldable) as List
import Data.Map (fromFoldable, isEmpty, keys, toUnfoldable, union) as Map
import Data.Profunctor (wrapIso)
import Data.Set (findMin, intersection) as Set
import Data.UUID (UUID)
import Data.Validation.Semigroup (validation)
import DelegateServer.AppManager.Types (AuctionSlot)
import DelegateServer.Handlers.HostAuction (HostAuctionError)
import DelegateServer.Handlers.ReserveSlot (ReserveSlotError)
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  )
import Effect.Aff.Class (liftAff)
import HydraAuctionOffchain.Codec (orefCodec, txHashCodec)
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
  , AuctionTerms(AuctionTerms)
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
import HydraAuctionOffchain.Service.Common (ServiceError)
import HydraAuctionOffchain.Service.DelegateServer
  ( getAvailableSlotsRequest
  , hostAuctionRequest
  , reserveSlotRequest
  )
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
      { txHash: txHashCodec
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
  network <- lift getNetworkId

  -- Get seller pkh and vkey:
  { vkey, address, pkh: sellerPkh } <-
    withExceptT AnnounceAuction_Error_CouldNotGetOwnPubKey
      askWalletVk

  -- Convert seller address:
  sellerAddressPlutus <- Plutus.Address.fromCardano address
    ?? AnnounceAuction_Error_SellerAddressConversionFailure

  -- Reserve delegate group slot:
  slotInfo <-
    maybe
      (pure mempty)
      (ExceptT <<< liftAff <<< reserveSlot <<< _.httpServers <<< unwrap)
      params.delegateInfo

  -- Check auction terms:
  let
    delegates = _.delegatePkh <$> slotInfo
    auctionTerms@(AuctionTerms auctionTermsRec) =
      mkAuctionTerms params.auctionTerms sellerAddressPlutus vkey delegates
  validateAuctionTerms auctionTerms #
    validation (throwError <<< AnnounceAuction_Error_InvalidAuctionTerms) pure

  -- Convert auction lot Value:
  auctionLotValue <- Plutus.Value.toCardano auctionTermsRec.auctionLot
    ?? AnnounceAuction_Error_AuctionLotValueConversionFailure

  -- Select utxos to cover auction lot Value:
  utxos <- getWalletUtxos !? AnnounceAuction_Error_CouldNotGetWalletUtxos
  additionalAuctionLotUtxos <- queryUtxos params.additionalAuctionLotOrefs
    !? AnnounceAuction_Error_CouldNotGetAdditionalAuctionLotUtxos
  let utxos' = Map.union utxos additionalAuctionLotUtxos
  auctionLotUtxos <- selectUtxos utxos' auctionLotValue
    !? AnnounceAuction_Error_CouldNotCoverAuctionLot
  when (Map.isEmpty auctionLotUtxos) $
    throwError AnnounceAuction_Error_EmptyAuctionLotUtxoMap -- impossible

  -- Select nonce utxo from the auction lot utxos:
  let nonceUtxo = unsafePartial fromJust $ Array.head $ Map.toUnfoldable auctionLotUtxos
  let nonceOref = fst nonceUtxo

  -- Check that the current time < bidding start time:
  nowTime <- lift currentTime
  unless (nowTime < auctionTermsRec.biddingStart) $
    throwError AnnounceAuction_Error_CurrentTimeAfterBiddingStart

  -- Get auction minting policy:
  auctionMetadataSh <- lift $ PlutusScript.hash <$> mkAuctionMetadataValidator
  auctionMintingPolicy <- lift $ mkAuctionMintingPolicy auctionMetadataSh nonceOref
  let auctionCs = PlutusScript.hash auctionMintingPolicy

  -- Get auction validators:
  validators <-
    withExceptT AnnounceAuction_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms
  let validatorHashes = PlutusScript.hash <$> validators
  let validatorAddresses = unwrap $ flip scriptHashAddress Nothing <<< wrap <$> validatorHashes

  -- Get auction metadata validator hash:
  metadataValidatorHash <- lift $ PlutusScript.hash <$> mkAuctionMetadataValidator
  let
    mkAuctionToken :: AssetName -> Value
    mkAuctionToken tokenName = Value.singleton auctionCs tokenName BigNum.one

    auctionTokenBundle :: Value
    auctionTokenBundle =
      -- safe, no numeric overflow is possible here 
      unsafePartial $ foldMap mkAuctionToken
        [ auctionEscrowTokenName
        , auctionMetadataTokenName
        , standingBidTokenName
        ]

    mintAuctionRedeemer :: RedeemerDatum
    mintAuctionRedeemer = wrap $ toData MintAuction

    -- AuctionEscrow -------------------------------------------------

    auctionEscrowValidatorHash :: ScriptHash
    auctionEscrowValidatorHash = (unwrap validatorHashes).auctionEscrow

    auctionEscrowValue :: Value
    auctionEscrowValue =
      unsafePartial do
        auctionLotValue <> mkAuctionToken auctionEscrowTokenName <>
          mkAuctionToken standingBidTokenName

    auctionEscrowDatum :: PlutusData
    auctionEscrowDatum = toData AuctionAnnounced

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

    auctionInfoDatum :: PlutusData
    auctionInfoDatum = toData auctionInfo

    -- AuctionActor --------------------------------------------------

    sellerOracle :: PersonalOracle
    sellerOracle = mkPersonalOracle network $ wrap sellerPkh

    sellerOracleTokenValue :: Value
    sellerOracleTokenValue = assetToValue sellerOracle.assetClass BigNum.one

    auctionActorDatum :: PlutusData
    auctionActorDatum = toData $ AuctionActor
      { auctionInfo: mkAuctionInfoExtended auctionInfo Nothing
      , role: ActorRoleSeller
      }

    --

    txValidRange :: POSIXTimeRange
    txValidRange = to $ auctionTermsRec.biddingStart - wrap (BigInt.fromInt 1000)

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Spend auction lot utxos, including nonce utxo:
        foldMap Constraints.mustSpendPubKeyOutput $ Map.keys auctionLotUtxos

      -- Mint auction state, auction metadata, and standing bid tokens:
      , Constraints.mustMintValueWithRedeemer mintAuctionRedeemer $ Mint.fromMultiAsset $
          Value.getMultiAsset auctionTokenBundle

      -- Lock auction lot with auction state and standing bid tokens
      -- at the auction escrow validator address, set state to 
      -- AuctionAnnounced:
      , Constraints.mustPayToScript auctionEscrowValidatorHash auctionEscrowDatum DatumInline
          auctionEscrowValue

      -- Mint seller's personal oracle token:
      , Constraints.mustMintCurrencyUsingNativeScript sellerOracle.nativeScript
          (unwrap sellerOracle.assetClass).tokenName
          Cardano.Int.one

      -- Lock auction actor datum with personal oracle token at 
      -- the seller's personal oracle address:
      , Constraints.mustPayToScript sellerOracle.nativeScriptHash auctionActorDatum
          DatumInline
          sellerOracleTokenValue

      -- Lock auction metadata datum and auction metadata token 
      -- at the auction metadata validator address:
      , Constraints.mustPayToScriptWithScriptRef metadataValidatorHash auctionInfoDatum
          DatumInline
          -- Attach standing bid reference script to meet the max tx
          -- size requirement for subsequent transactions.
          (PlutusScriptRef $ (unwrap validators).standingBid)
          auctionInfoValue

      -- Set transaction validity interval to registration period:
      , Constraints.mustValidateIn txValidRange
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs auctionLotUtxos
      , Lookups.plutusMintingPolicy auctionMintingPolicy
      ]

  result@{ txHash } <- lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }
  lift $ awaitTxConfirmed txHash

  -- Send requests to the delegate group to host the auction:
  let auctionMetadataOref = wrap { transactionId: txHash, index: zero }
  ExceptT $ liftAff $ hostAuction auctionMetadataOref slotInfo

  pure $ Record.merge result
    { auctionInfo:
        mkAuctionInfoExtended auctionInfo $ Just $ wrap
          { transactionId: result.txHash
          , index: zero
          }
    }

selectUtxos :: UtxoMap -> Value -> Contract (Maybe UtxoMap)
selectUtxos utxos requiredValue = do
  let utxoIndex = buildUtxoIndex utxos
  hush <$> runExceptT do
    selState <- performMultiAssetSelection SelectionStrategyMinimal utxoIndex
      (Val.fromValue requiredValue)
    let selectedUtxos = (unwrap selState).selectedUtxos
    pure selectedUtxos

queryUtxos :: Array TransactionInput -> Contract (Maybe UtxoMap)
queryUtxos = map (map Map.fromFoldable <<< sequence) <<< parTraverse getUtxo'
  where
  getUtxo'
    :: TransactionInput
    -> Contract (Maybe (TransactionInput /\ TransactionOutput))
  getUtxo' oref = map (Tuple oref) <$> getUtxo oref

----------------------------------------------------------------------
-- delegate-server requests

type DelegateServerSlotInfo =
  { httpServer :: String
  , slot :: AuctionSlot
  , reservationCode :: UUID
  , delegatePkh :: Ed25519KeyHash
  }

hostAuction
  :: TransactionInput
  -> Array DelegateServerSlotInfo
  -> Aff (Either AnnounceAuctionContractError Unit)
hostAuction auctionMetadataOref =
  runExceptT <<< traverse_ \({ httpServer, slot, reservationCode }) -> do
    resp <- ExceptT $ lmap AnnounceAuction_Error_HostAuctionRequestFailed <$>
      hostAuctionRequest httpServer
        { auctionMetadataOref
        , slot
        , reservationCode: Just reservationCode
        }
    case resp of
      ServerResponseError err ->
        throwError $ AnnounceAuction_Error_CouldNotHostAuction err
      ServerResponseSuccess _ ->
        pure unit

reserveSlot
  :: Array String
  -> Aff (Either AnnounceAuctionContractError (Array DelegateServerSlotInfo))
reserveSlot httpServers = runExceptT do
  mAvailableSlot <- ExceptT $ lmap AnnounceAuction_Error_GetAvailableSlotsRequestFailed <$>
    getDelegateGroupSlot httpServers
  slot <- mAvailableSlot ?? AnnounceAuction_Error_NoAvailableDelegateGroupSlots
  foldM
    ( \acc httpServer -> do
        resp <- ExceptT $ lmap AnnounceAuction_Error_ReserveSlotRequestFailed <$>
          reserveSlotRequest httpServer { slot }
        case resp of
          ServerResponseError err ->
            throwError $
              AnnounceAuction_Error_CouldNotReserveDelegateGroupSlot slot err
          ServerResponseSuccess { reservationCode, delegatePkh } ->
            pure $ Array.cons
              { httpServer
              , slot
              , reservationCode
              , delegatePkh
              }
              acc
    )
    mempty
    (List.fromFoldable httpServers)

getDelegateGroupSlot :: Array String -> Aff (Either ServiceError (Maybe AuctionSlot))
getDelegateGroupSlot httpServers =
  runExceptT do
    sets <- parTraverse (ExceptT <<< getAvailableSlotsRequest) httpServers
    pure $ maybe
      Nothing
      (\{ head, tail } -> Set.findMin $ foldl Set.intersection head tail)
      (Array.uncons sets)

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
  | AnnounceAuction_Error_SellerAddressConversionFailure
  | AnnounceAuction_Error_AuctionLotValueConversionFailure
  | AnnounceAuction_Error_GetAvailableSlotsRequestFailed ServiceError
  | AnnounceAuction_Error_NoAvailableDelegateGroupSlots
  | AnnounceAuction_Error_ReserveSlotRequestFailed ServiceError
  | AnnounceAuction_Error_CouldNotReserveDelegateGroupSlot AuctionSlot ReserveSlotError
  | AnnounceAuction_Error_HostAuctionRequestFailed ServiceError
  | AnnounceAuction_Error_CouldNotHostAuction HostAuctionError

derive instance Generic AnnounceAuctionContractError _

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
      "Could not build auction validators, error: "
        <> show err
        <> "."

    AnnounceAuction_Error_CouldNotGetOwnPubKey err ->
      "Could not get own public key, error: "
        <> show err
        <> "."

    AnnounceAuction_Error_SellerAddressConversionFailure ->
      "Could not convert seller address to Plutus.Address."

    AnnounceAuction_Error_AuctionLotValueConversionFailure ->
      "Could not convert auction lot Plutus.Value to Cardano.Value."

    AnnounceAuction_Error_GetAvailableSlotsRequestFailed serviceErr ->
      "Could not get available slots, request failed: "
        <> show serviceErr
        <> "."

    AnnounceAuction_Error_NoAvailableDelegateGroupSlots ->
      "The delegate group currently has no available slots."

    AnnounceAuction_Error_ReserveSlotRequestFailed serviceErr ->
      "Could not reserve delegate group slot, request failed: "
        <> show serviceErr
        <> "."

    AnnounceAuction_Error_CouldNotReserveDelegateGroupSlot slot err ->
      "Could not reserve delegate group slot "
        <> show slot
        <> ", delegate-server error: "
        <> show err
        <> "."

    AnnounceAuction_Error_HostAuctionRequestFailed serviceErr ->
      "Failed to host the auction, request failed: "
        <> show serviceErr
        <> "."

    AnnounceAuction_Error_CouldNotHostAuction err ->
      "Failed to host the auction, delegate-server error: "
        <> show err
        <> "."
