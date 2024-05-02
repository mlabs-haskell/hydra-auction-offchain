module HydraAuctionOffchain.Contract.ClaimAuctionLotSeller
  ( ClaimAuctionLotSellerContractError
      ( ClaimAuctionLotSeller_Error_InvalidAuctionTerms
      , ClaimAuctionLotSeller_Error_CurrentTimeBeforePurchaseDeadline
      , ClaimAuctionLotSeller_Error_CouldNotBuildAuctionValidators
      , ClaimAuctionLotSeller_Error_InvalidAuctionInfo
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
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustPayToPubKey
  , mustPayToScript
  , mustSpendScriptOutput
  ) as Constraints
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
  , AuctionInfo(AuctionInfo)
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

claimAuctionLotSellerContract :: AuctionInfo -> Contract (ContractOutput TransactionHash)
claimAuctionLotSellerContract =
  mkContractOutput _.txHash <<< mkClaimAuctionLotSellerContractWithErrors

mkClaimAuctionLotSellerContractWithErrors
  :: AuctionInfo
  -> ExceptT ClaimAuctionLotSellerContractError Contract ContractResult
mkClaimAuctionLotSellerContractWithErrors auctionInfo = do
  let
    AuctionInfo auctionInfoRec = auctionInfo
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
  validateAuctionInfo auctionInfo validators #
    validation (throwError <<< ClaimAuctionLotSeller_Error_InvalidAuctionInfo) pure

  -- Query current auction escrow utxo:
  auctionEscrowUtxo <- queryAuctionEscrowUtxo BiddingStarted auctionInfo
    !? ClaimAuctionLotSeller_Error_CouldNotFindAuctionEscrowUtxo

  -- Query standing bid utxo:
  standingBidUtxo /\ standingBid <- queryStandingBidUtxo auctionInfo
    !? ClaimAuctionLotSeller_Error_CouldNotFindStandingBidUtxo

  -- Query bidder deposit utxo:
  let mBidderInfo = unwrap standingBid <#> _.bidder <<< unwrap
  mBidderDepositUtxo <- lift $ maybe (pure Nothing) (queryBidderDepositUtxo auctionInfo)
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

    --

    totalAuctionFeesValue :: Value
    totalAuctionFeesValue = Value.lovelaceValueOf $ totalAuctionFees auctionTerms

    constraints :: TxConstraints Void Void
    constraints = mconcat
      [ -- Spend auction escrow utxo:
        Constraints.mustSpendScriptOutput auctionEscrowOref auctionEscrowRedeemer

      , -- Spend standing bid utxo:
        Constraints.mustSpendScriptOutput standingBidOref standingBidRedeemer

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
      ]

    lookups :: ScriptLookups Void
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.fromFoldable
          ( [ auctionEscrowUtxo, standingBidUtxo ]
              <> maybe mempty Array.singleton mBidderDepositUtxo
          )
      , Lookups.validator (unwrap validators).auctionEscrow
      , Lookups.validator (unwrap validators).standingBid
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
  | ClaimAuctionLotSeller_Error_CouldNotFindAuctionEscrowUtxo
  | ClaimAuctionLotSeller_Error_CouldNotFindStandingBidUtxo
  | ClaimAuctionLotSeller_Error_CouldNotFindBidderDepositUtxo
  | ClaimAuctionLotSeller_Error_CouldNotGetSellerPkh

derive instance Generic ClaimAuctionLotSellerContractError _
derive instance Eq ClaimAuctionLotSellerContractError

instance Show ClaimAuctionLotSellerContractError where
  show = genericShow

instance ToContractError ClaimAuctionLotSellerContractError where
  toContractError = wrap <<< case _ of
    ClaimAuctionLotSeller_Error_InvalidAuctionTerms errors ->
      { errorCode: "ClaimAuctionLotSeller01"
      , message: "Invalid auction terms, errors: " <> show errors <> "."
      }
    ClaimAuctionLotSeller_Error_CurrentTimeBeforePurchaseDeadline ->
      { errorCode: "ClaimAuctionLotSeller02"
      , message: "Tx cannot be submitted before purchase deadline."
      }
    ClaimAuctionLotSeller_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "ClaimAuctionLotSeller03"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
    ClaimAuctionLotSeller_Error_InvalidAuctionInfo errors ->
      { errorCode: "ClaimAuctionLotSeller04"
      , message: "Invalid auction info, errors: " <> show errors <> "."
      }
    ClaimAuctionLotSeller_Error_CouldNotFindAuctionEscrowUtxo ->
      { errorCode: "ClaimAuctionLotSeller05"
      , message: "Could not find auction escrow utxo."
      }
    ClaimAuctionLotSeller_Error_CouldNotFindStandingBidUtxo ->
      { errorCode: "ClaimAuctionLotSeller06"
      , message: "Could not find standing bid utxo."
      }
    ClaimAuctionLotSeller_Error_CouldNotFindBidderDepositUtxo ->
      { errorCode: "ClaimAuctionLotSeller07"
      , message: "Could not find bidder deposit utxo."
      }
    ClaimAuctionLotSeller_Error_CouldNotGetSellerPkh ->
      { errorCode: "ClaimAuctionLotSeller08"
      , message: "Could not get seller pkh."
      }
