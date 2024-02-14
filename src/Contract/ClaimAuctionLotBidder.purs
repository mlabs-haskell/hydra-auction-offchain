module HydraAuctionOffchain.Contract.ClaimAuctionLotBidder
  ( ClaimAuctionLotBidderContractError
      ( ClaimAuctionLotBidder_Error_InvalidAuctionTerms
      , ClaimAuctionLotBidder_Error_CurrentTimeBeforeBiddingEnd
      , ClaimAuctionLotBidder_Error_CurrentTimeAfterPurchaseDeadline
      , ClaimAuctionLotBidder_Error_CouldNotBuildAuctionValidators
      , ClaimAuctionLotBidder_Error_InvalidAuctionInfo
      , ClaimAuctionLotBidder_Error_CouldNotFindAuctionEscrowUtxo
      , ClaimAuctionLotBidder_Error_CouldNotFindStandingBidUtxo
      , ClaimAuctionLotBidder_Error_EmptyStandingBid
      , ClaimAuctionLotBidder_Error_CouldNotGetBuyerPkh
      , ClaimAuctionLotBidder_Error_CouldNotGetSellerPkh
      )
  , claimAuctionLotBidderContract
  , mkClaimAuctionLotBidderContractWithErrors
  ) where

import Contract.Prelude

import Contract.Address (toPubKeyHash)
import Contract.Chain (currentTime)
import Contract.Log (logWarn')
import Contract.Monad (Contract)
import Contract.PlutusData (Datum, Redeemer, toData, unitDatum)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTimeRange, mkFiniteInterval)
import Contract.Transaction (TransactionHash, TransactionInput, TransactionOutput)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustPayToPubKey
  , mustPayToScript
  , mustSpendScriptOutput
  , mustValidateIn
  ) as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (lovelaceValueOf, singleton, valueOf) as Value
import Control.Error.Util (bool, (!?), (??))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, singleton) as Array
import Data.BigInt (fromInt) as BigInt
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Contract.MintingPolicies
  ( auctionEscrowTokenName
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.StartBidding (queryAuctionEscrowUtxo)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionEscrowRedeemer(BidderBuysRedeemer)
  , AuctionEscrowState(BiddingStarted, AuctionConcluded)
  , AuctionInfo(AuctionInfo)
  , AuctionInfoValidationError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , BidderDepositRedeemer(UseDepositWinnerRedeemer)
  , BidderInfo
  , ContractOutput
  , ContractResult
  , StandingBidRedeemer(ConcludeAuctionRedeemer)
  , StandingBidState
  , Utxo
  , emptySubmitTxData
  , mkContractOutput
  , sellerPayout
  , submitTxReturningContractResult
  , totalAuctionFees
  , validateAuctionInfo
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Helpers (getInlineDatum)

claimAuctionLotBidderContract :: AuctionInfo -> Contract (ContractOutput TransactionHash)
claimAuctionLotBidderContract =
  mkContractOutput _.txHash <<< mkClaimAuctionLotBidderContractWithErrors

mkClaimAuctionLotBidderContractWithErrors
  :: AuctionInfo
  -> ExceptT ClaimAuctionLotBidderContractError Contract ContractResult
mkClaimAuctionLotBidderContractWithErrors auctionInfo = do
  let
    AuctionInfo auctionInfoRec = auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< ClaimAuctionLotBidder_Error_InvalidAuctionTerms) pure

  -- Check that the current time is within the purchase period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingEnd) $
    throwError ClaimAuctionLotBidder_Error_CurrentTimeBeforeBiddingEnd
  when (nowTime >= auctionTermsRec.purchaseDeadline) $
    throwError ClaimAuctionLotBidder_Error_CurrentTimeAfterPurchaseDeadline

  -- Build validators:
  validators <-
    withExceptT ClaimAuctionLotBidder_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  -- Check auction info:
  validateAuctionInfo auctionInfo validators #
    validation (throwError <<< ClaimAuctionLotBidder_Error_InvalidAuctionInfo) pure

  -- Query current auction escrow utxo:
  auctionEscrowUtxo <- queryAuctionEscrowUtxo BiddingStarted auctionInfo
    !? ClaimAuctionLotBidder_Error_CouldNotFindAuctionEscrowUtxo

  -- Query standing bid utxo:
  standingBidUtxo /\ standingBid <- queryStandingBidUtxo auctionInfo
    !? ClaimAuctionLotBidder_Error_CouldNotFindStandingBidUtxo

  -- Get bid terms:
  bidTerms <- unwrap standingBid ?? ClaimAuctionLotBidder_Error_EmptyStandingBid
  let bidderInfo = (unwrap bidTerms).bidder

  -- Query bidder deposit utxo:
  mBidderDepositUtxo <- lift $ queryBidderDepositUtxo auctionInfo bidderInfo
  when (isNothing mBidderDepositUtxo) do
    lift $ logWarn' "Could not find bidder deposit utxo."

  -- Get buyer pkh:
  buyerPkh <- wrap <$> toPubKeyHash (unwrap bidderInfo).bidderAddress
    ?? ClaimAuctionLotBidder_Error_CouldNotGetBuyerPkh

  -- Get seller pkh:
  sellerPkh <- wrap <$> toPubKeyHash auctionTermsRec.sellerAddress
    ?? ClaimAuctionLotBidder_Error_CouldNotGetSellerPkh

  let
    validatorHashes = unwrap $ validatorHash <$> validators

    mkAuctionToken :: TokenName -> Value
    mkAuctionToken tokenName = Value.singleton auctionCs tokenName one

    -- AuctionEscrow -------------------------------------------------

    auctionEscrowOref :: TransactionInput
    auctionEscrowOref = fst auctionEscrowUtxo

    auctionEscrowRedeemer :: Redeemer
    auctionEscrowRedeemer = wrap $ toData BidderBuysRedeemer

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
    bidderDepositRedeemer = wrap $ toData UseDepositWinnerRedeemer

    --

    totalAuctionFeesValue :: Value
    totalAuctionFeesValue = Value.lovelaceValueOf $ totalAuctionFees auctionTerms

    sellerPayoutValue :: Value
    sellerPayoutValue = Value.lovelaceValueOf $ sellerPayout auctionTerms bidTerms

    txValidRange :: POSIXTimeRange
    txValidRange =
      mkFiniteInterval nowTime
        (auctionTermsRec.purchaseDeadline - wrap (BigInt.fromInt 1000))

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

      , -- Send auction lot to the buyer:
        Constraints.mustPayToPubKey buyerPkh auctionTermsRec.auctionLot

      , -- Send auction proceeds to the seller:
        Constraints.mustPayToPubKey sellerPkh sellerPayoutValue

      , -- Lock total auction fees at fee escrow validator address:
        Constraints.mustPayToScript validatorHashes.feeEscrow unitDatum DatumInline
          totalAuctionFeesValue

      , -- This transaction must be signed by the buyer:
        Constraints.mustBeSignedBy buyerPkh

      , -- Set transaction validity interval to purchase period:
        Constraints.mustValidateIn txValidRange
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

queryStandingBidUtxo :: AuctionInfo -> Contract (Maybe (Utxo /\ StandingBidState))
queryStandingBidUtxo (AuctionInfo auctionInfo) =
  utxosAt auctionInfo.standingBidAddr <#> \utxos -> do
    let getTxOut = _.output <<< unwrap <<< snd
    standingBidUtxo <- Array.find (hasStandingBidToken <<< getTxOut) $ Map.toUnfoldable utxos
    Tuple standingBidUtxo <$> getInlineDatum (getTxOut standingBidUtxo)
  where
  auctionCs :: CurrencySymbol
  auctionCs = auctionInfo.auctionId

  hasStandingBidToken :: TransactionOutput -> Boolean
  hasStandingBidToken txOut =
    Value.valueOf (unwrap txOut).amount auctionCs standingBidTokenName == one

queryBidderDepositUtxo :: AuctionInfo -> BidderInfo -> Contract (Maybe Utxo)
queryBidderDepositUtxo (AuctionInfo auctionInfo) bidderInfo =
  utxosAt auctionInfo.bidderDepositAddr
    <#> Array.find (eq (Just bidderInfo) <<< getInlineDatum <<< _.output <<< unwrap <<< snd)
    <<< Map.toUnfoldable

----------------------------------------------------------------------
-- Errors

data ClaimAuctionLotBidderContractError
  = ClaimAuctionLotBidder_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | ClaimAuctionLotBidder_Error_CurrentTimeBeforeBiddingEnd
  | ClaimAuctionLotBidder_Error_CurrentTimeAfterPurchaseDeadline
  | ClaimAuctionLotBidder_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | ClaimAuctionLotBidder_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)
  | ClaimAuctionLotBidder_Error_CouldNotFindAuctionEscrowUtxo
  | ClaimAuctionLotBidder_Error_CouldNotFindStandingBidUtxo
  | ClaimAuctionLotBidder_Error_EmptyStandingBid
  | ClaimAuctionLotBidder_Error_CouldNotGetBuyerPkh
  | ClaimAuctionLotBidder_Error_CouldNotGetSellerPkh

derive instance Generic ClaimAuctionLotBidderContractError _
derive instance Eq ClaimAuctionLotBidderContractError

instance Show ClaimAuctionLotBidderContractError where
  show = genericShow

instance ToContractError ClaimAuctionLotBidderContractError where
  toContractError = wrap <<< case _ of
    ClaimAuctionLotBidder_Error_InvalidAuctionTerms errors ->
      { errorCode: "ClaimAuctionLotBidder01"
      , message: "Invalid auction terms, errors: " <> show errors <> "."
      }
    ClaimAuctionLotBidder_Error_CurrentTimeBeforeBiddingEnd ->
      { errorCode: "ClaimAuctionLotBidder02"
      , message: "Tx cannot be submitted before bidding end time."
      }
    ClaimAuctionLotBidder_Error_CurrentTimeAfterPurchaseDeadline ->
      { errorCode: "ClaimAuctionLotBidder03"
      , message: "Tx cannot be submitted after purchase deadline."
      }
    ClaimAuctionLotBidder_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "ClaimAuctionLotBidder04"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
    ClaimAuctionLotBidder_Error_InvalidAuctionInfo errors ->
      { errorCode: "ClaimAuctionLotBidder05"
      , message: "Invalid auction info, errors: " <> show errors <> "."
      }
    ClaimAuctionLotBidder_Error_CouldNotFindAuctionEscrowUtxo ->
      { errorCode: "ClaimAuctionLotBidder06"
      , message: "Could not find auction escrow utxo."
      }
    ClaimAuctionLotBidder_Error_CouldNotFindStandingBidUtxo ->
      { errorCode: "ClaimAuctionLotBidder07"
      , message: "Could not find standing bid utxo."
      }
    ClaimAuctionLotBidder_Error_EmptyStandingBid ->
      { errorCode: "ClaimAuctionLotBidder08"
      , message: "Standing bid is empty."
      }
    ClaimAuctionLotBidder_Error_CouldNotGetBuyerPkh ->
      { errorCode: "ClaimAuctionLotBidder09"
      , message: "Could not get buyer pkh."
      }
    ClaimAuctionLotBidder_Error_CouldNotGetSellerPkh ->
      { errorCode: "ClaimAuctionLotBidder10"
      , message: "Could not get seller pkh."
      }
