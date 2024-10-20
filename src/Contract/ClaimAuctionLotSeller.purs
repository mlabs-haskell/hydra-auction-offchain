module HydraAuctionOffchain.Contract.ClaimAuctionLotSeller
  ( ClaimAuctionLotSellerContractError
      ( ClaimAuctionLotSeller_Error_InvalidAuctionTerms
      , ClaimAuctionLotSeller_Error_CurrentTimeBeforePurchaseDeadline
      , ClaimAuctionLotSeller_Error_CouldNotBuildAuctionValidators
      , ClaimAuctionLotSeller_Error_InvalidAuctionInfo
      , ClaimAuctionLotSeller_Error_MissingMetadataOref
      , ClaimAuctionLotSeller_Error_CouldNotQueryAuctionMetadataUtxo
      , ClaimAuctionLotSeller_Error_CouldNotFindAuctionEscrowUtxo
      , ClaimAuctionLotSeller_Error_CouldNotFindStandingBidUtxo
      , ClaimAuctionLotSeller_Error_CouldNotFindBidderDepositUtxo
      , ClaimAuctionLotSeller_Error_CouldNotGetSellerPkh
      )
  , claimAuctionLotSellerContract
  , mkClaimAuctionLotSellerContractWithErrors
  ) where

import Contract.Prelude

import Contract.Address (toPubKeyHash)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, Redeemer, toData, unitDatum)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (from)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionUnspentOutput
  , mkTxUnspentOut
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustPayToPubKey
  , mustPayToScript
  , mustSpendScriptOutput
  , mustSpendScriptOutputUsingScriptRef
  , mustValidateIn
  ) as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (TokenName, Value)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Control.Error.Util (bool, (!?), (??))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (singleton) as Array
import Data.Map (fromFoldable) as Map
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Contract.MintingPolicies
  ( auctionEscrowTokenName
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.QueryUtxo
  ( queryAuctionEscrowUtxo
  , queryBidderDepositUtxo
  , queryStandingBidUtxo
  )
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionEscrowRedeemer(SellerReclaimsRedeemer)
  , AuctionEscrowState(BiddingStarted, AuctionConcluded)
  , AuctionInfoExtended(AuctionInfoExtended)
  , AuctionInfoValidationError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , BidderDepositRedeemer(ClaimDepositSellerRedeemer)
  , ContractOutput
  , ContractResult
  , StandingBidRedeemer(ConcludeAuctionRedeemer)
  , emptySubmitTxData
  , mkContractOutput
  , submitTxReturningContractResult
  , totalAuctionFees
  , validateAuctionInfo
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Helpers (withEmptyPlutusV2Script)

claimAuctionLotSellerContract
  :: AuctionInfoExtended -> Contract (ContractOutput TransactionHash)
claimAuctionLotSellerContract =
  mkContractOutput _.txHash <<< mkClaimAuctionLotSellerContractWithErrors

mkClaimAuctionLotSellerContractWithErrors
  :: AuctionInfoExtended
  -> ExceptT ClaimAuctionLotSellerContractError Contract ContractResult
mkClaimAuctionLotSellerContractWithErrors auctionInfo = do
  let
    AuctionInfoExtended auctionInfoRec = auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< ClaimAuctionLotSeller_Error_InvalidAuctionTerms) pure

  -- Check that the current time is after the purchase deadline:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.purchaseDeadline) $
    throwError ClaimAuctionLotSeller_Error_CurrentTimeBeforePurchaseDeadline

  -- Build validators:
  validators <-
    withExceptT ClaimAuctionLotSeller_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  -- Check auction info:
  validateAuctionInfo auctionInfoRec validators #
    validation (throwError <<< ClaimAuctionLotSeller_Error_InvalidAuctionInfo) pure

  -- Query auction metadata utxo:
  auctionMetadataOref <- auctionInfoRec.metadataOref
    ?? ClaimAuctionLotSeller_Error_MissingMetadataOref
  auctionMetadataTxOut <- getUtxo auctionMetadataOref
    !? ClaimAuctionLotSeller_Error_CouldNotQueryAuctionMetadataUtxo

  -- Query current auction escrow utxo:
  auctionEscrowUtxo <- queryAuctionEscrowUtxo BiddingStarted auctionInfoRec
    !? ClaimAuctionLotSeller_Error_CouldNotFindAuctionEscrowUtxo

  -- Query standing bid utxo:
  standingBidUtxo /\ standingBid <- queryStandingBidUtxo auctionInfoRec
    !? ClaimAuctionLotSeller_Error_CouldNotFindStandingBidUtxo

  -- Query bidder deposit utxo:
  let mBidderInfo = unwrap standingBid <#> _.bidder <<< unwrap
  mBidderDepositUtxo <- lift $ maybe (pure Nothing) (queryBidderDepositUtxo auctionInfoRec)
    mBidderInfo
  when (isJust mBidderInfo && isNothing mBidderDepositUtxo) $
    throwError ClaimAuctionLotSeller_Error_CouldNotFindBidderDepositUtxo

  -- Get seller pkh:
  sellerPkh <- wrap <$> toPubKeyHash auctionTermsRec.sellerAddress
    ?? ClaimAuctionLotSeller_Error_CouldNotGetSellerPkh

  let
    validatorHashes = unwrap $ validatorHash <$> validators

    mkAuctionToken :: TokenName -> Value
    mkAuctionToken tokenName = Value.singleton auctionCs tokenName one

    -- AuctionEscrow -------------------------------------------------

    auctionEscrowOref :: TransactionInput
    auctionEscrowOref = fst auctionEscrowUtxo

    auctionEscrowRedeemer :: Redeemer
    auctionEscrowRedeemer = wrap $ toData SellerReclaimsRedeemer

    auctionEscrowDatum :: Datum
    auctionEscrowDatum = wrap $ toData AuctionConcluded

    auctionEscrowValue :: Value
    auctionEscrowValue = mkAuctionToken auctionEscrowTokenName
      <> mkAuctionToken standingBidTokenName

    -- StandingBid ---------------------------------------------------

    standingBidOref :: TransactionInput
    standingBidOref = fst standingBidUtxo

    standingBidRedeemer :: Redeemer
    standingBidRedeemer = wrap $ toData ConcludeAuctionRedeemer

    -- BidderDeposit -------------------------------------------------

    mBidderDepositOref :: Maybe TransactionInput
    mBidderDepositOref = fst <$> mBidderDepositUtxo

    bidderDepositRedeemer :: Redeemer
    bidderDepositRedeemer = wrap $ toData ClaimDepositSellerRedeemer

    -- AuctionMetadata -----------------------------------------------

    auctionMetadataUtxo :: TransactionUnspentOutput
    auctionMetadataUtxo = mkTxUnspentOut auctionMetadataOref $ withEmptyPlutusV2Script
      auctionMetadataTxOut

    --

    totalAuctionFeesValue :: Value
    totalAuctionFeesValue = Value.lovelaceValueOf $ totalAuctionFees auctionTerms

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Spend auction escrow utxo:
        Constraints.mustSpendScriptOutput auctionEscrowOref auctionEscrowRedeemer

      , -- Spend standing bid utxo:
        Constraints.mustSpendScriptOutputUsingScriptRef standingBidOref standingBidRedeemer
          (RefInput auctionMetadataUtxo)

      , -- Spend bidder deposit utxo, if present:
        mBidderDepositOref # maybe mempty
          (flip Constraints.mustSpendScriptOutput bidderDepositRedeemer)

      , -- Lock auction escrow and standing bid tokens at auction
        -- escrow validator address:
        Constraints.mustPayToScript validatorHashes.auctionEscrow auctionEscrowDatum
          DatumInline
          auctionEscrowValue

      , -- Send auction lot to the seller:
        Constraints.mustPayToPubKey sellerPkh auctionTermsRec.auctionLot

      , -- Lock total auction fees at fee escrow validator address:
        Constraints.mustPayToScript validatorHashes.feeEscrow unitDatum DatumInline
          totalAuctionFeesValue

      , -- This transaction must be signed by the seller:
        Constraints.mustBeSignedBy sellerPkh

      , -- Set transaction validity interval (post-purchase period):
        Constraints.mustValidateIn $ from nowTime
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.fromFoldable
          ( [ auctionEscrowUtxo, standingBidUtxo ]
              <> maybe mempty Array.singleton mBidderDepositUtxo
          )
      , Lookups.validator (unwrap validators).auctionEscrow
      , isJust mBidderDepositUtxo # bool mempty
          (Lookups.validator (unwrap validators).bidderDeposit)
      ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

----------------------------------------------------------------------
-- Errors 

data ClaimAuctionLotSellerContractError
  = ClaimAuctionLotSeller_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | ClaimAuctionLotSeller_Error_CurrentTimeBeforePurchaseDeadline
  | ClaimAuctionLotSeller_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | ClaimAuctionLotSeller_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)
  | ClaimAuctionLotSeller_Error_MissingMetadataOref
  | ClaimAuctionLotSeller_Error_CouldNotQueryAuctionMetadataUtxo
  | ClaimAuctionLotSeller_Error_CouldNotFindAuctionEscrowUtxo
  | ClaimAuctionLotSeller_Error_CouldNotFindStandingBidUtxo
  | ClaimAuctionLotSeller_Error_CouldNotFindBidderDepositUtxo
  | ClaimAuctionLotSeller_Error_CouldNotGetSellerPkh

derive instance Generic ClaimAuctionLotSellerContractError _
derive instance Eq ClaimAuctionLotSellerContractError

instance Show ClaimAuctionLotSellerContractError where
  show = genericShow

instance ToContractError ClaimAuctionLotSellerContractError where
  errorCodePrefix = const "ClaimAuctionLotSeller"
  errorMessage = case _ of
    ClaimAuctionLotSeller_Error_InvalidAuctionTerms errors ->
      "Invalid auction terms, errors: " <> show errors <> "."

    ClaimAuctionLotSeller_Error_CurrentTimeBeforePurchaseDeadline ->
      "Tx cannot be submitted before purchase deadline."

    ClaimAuctionLotSeller_Error_CouldNotBuildAuctionValidators err ->
      "Could not build auction validators, error: " <> show err <> "."

    ClaimAuctionLotSeller_Error_InvalidAuctionInfo errors ->
      "Invalid auction info, errors: " <> show errors <> "."

    ClaimAuctionLotSeller_Error_MissingMetadataOref ->
      "Auction metadata output reference not provided."

    ClaimAuctionLotSeller_Error_CouldNotQueryAuctionMetadataUtxo ->
      "Could not query auction metadata utxo."

    ClaimAuctionLotSeller_Error_CouldNotFindAuctionEscrowUtxo ->
      "Could not find auction escrow utxo."

    ClaimAuctionLotSeller_Error_CouldNotFindStandingBidUtxo ->
      "Could not find standing bid utxo."

    ClaimAuctionLotSeller_Error_CouldNotFindBidderDepositUtxo ->
      "Could not find bidder deposit utxo."

    ClaimAuctionLotSeller_Error_CouldNotGetSellerPkh ->
      "Could not get seller pkh."
