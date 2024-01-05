module HydraAuctionOffchain.Contract.PlaceBid where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.PlutusData (OutputDatum(OutputDatum), Redeemer, fromData, toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTimeRange, mkFiniteInterval)
import Contract.Transaction (TransactionInput, TransactionOutput(TransactionOutput))
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutput
  , mustValidateIn
  ) as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (flattenNonAdaAssets) as Value
import Control.Error.Util ((!?))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (findMap) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (singleton, toUnfoldable) as Map
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Contract.MintingPolicies (standingBidTokenName)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfo(AuctionInfo)
  , AuctionInfoValidationError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , ContractResult
  , StandingBidRedeemer(NewBidRedeemer)
  , StandingBidState
  , Utxo
  , validateAuctionInfo
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import Undefined (undefined)

newtype PlaceBidContractParams = PlaceBidContractParams
  { auctionInfo :: AuctionInfo
  , sellerSignature :: ByteArray
  , bidAmount :: BigInt
  }

mkPlaceBidContractWithErrors
  :: PlaceBidContractParams
  -> ExceptT PlaceBidContractError Contract ContractResult
mkPlaceBidContractWithErrors (PlaceBidContractParams params) = do
  let
    auctionInfo@(AuctionInfo auctionInfoRec) = params.auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check auction terms:
  validateAuctionTerms #
    validation (throwError <<< PlaceBid_Error_InvalidAuctionTerms) pure

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError PlaceBid_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError PlaceBid_Error_CurrentTimeAfterBiddingEnd

  -- Build validators:
  validators <-
    withExceptT PlaceBid_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  -- Check auction info:
  validateAuctionInfo auctionInfo validators #
    validation (throwError <<< PlaceBid_Error_InvalidAuctionInfo) pure

  -- Query current standing bid utxo:
  standingBidUtxo /\ oldBidState <- queryStandingBidUtxo auctionInfo
    !? PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo

  -- Get bidder signature:

  let
    validatorHashes = unwrap $ validatorHash <$> validators

    -- StandingBid ---------------------------------------------------

    standingBidOref :: TransactionInput
    standingBidOref = fst standingBidUtxo

    standingBidRedeemer :: Redeemer
    standingBidRedeemer = wrap $ toData NewBidRedeemer

    --

    txValidRange :: POSIXTimeRange
    txValidRange =
      mkFiniteInterval nowTime
        (auctionTermsRec.biddingEnd - wrap (BigInt.fromInt 1000))

    constraints :: TxConstraints Void Void
    constraints = mconcat
      [ -- Spend standing bid utxo with old standing bid datum: 
        Constraints.mustSpendScriptOutput standingBidOref standingBidRedeemer

      , -- Lock standing bid token with new standing bid datum at
        -- standing bid validator address:
        Constraints.mustPayToScript validatorHashes.standingBid standingBidDatum DatumInline
          standingBidTokenValue

      , -- This transaction must be signed by the bidder:
        Constraints.mustBeSignedBy bidderPkh

      , -- Set transaction validity interval to bidding period: 
        Constraints.mustValidateIn txValidRange
      ]

    lookups :: ScriptLookups Void
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.singleton standingBidOref $ snd standingBidUtxo
      , Lookups.validator (unwrap validators).standingBid
      ]

  lift $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }

queryStandingBidUtxo :: AuctionInfo -> Contract (Maybe (Utxo /\ StandingBidState))
queryStandingBidUtxo (AuctionInfo auctionInfo) =
  utxosAt auctionInfo.standingBidAddr
    <#> Array.findMap (currentStandingBidState <<< _.output <<< unwrap <<< snd)
    <<< Map.toUnfoldable
  where
  currentStandingBidState :: TransactionOutput -> Maybe StandingBidState
  currentStandingBidState (TransactionOutput txOut)
    | Value.flattenNonAdaAssets txOut.amount /=
        [ auctionInfo.auctionId /\ standingBidTokenName /\ one ] = Nothing
    | isJust txOut.referenceScript = Nothing
    | otherwise =
        case txOut.datum of
          OutputDatum datum -> fromData $ unwrap datum
          _ -> Nothing

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

data PlaceBidContractError
  = PlaceBid_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | PlaceBid_Error_CurrentTimeBeforeBiddingStart
  | PlaceBid_Error_CurrentTimeAfterBiddingEnd
  | PlaceBid_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | PlaceBid_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)
  | PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo

derive instance Generic PlaceBidContractError _
derive instance Eq PlaceBidContractError

instance Show PlaceBidContractError where
  show = genericShow

instance ToContractError where
  toContractError = wrap <<< case _ of
    PlaceBid_Error_InvalidAuctionTerms validationErrors ->
      { errorCode: "PlaceBid01"
      , message: "Invalid auction terms, errors: " <> show validationErrors <> "."
      }
    PlaceBid_Error_CurrentTimeBeforeBiddingStart ->
      { errorCode: "PlaceBid02"
      , message: "Tx cannot be submitted before bidding start time."
      }
    PlaceBid_Error_CurrentTimeAfterBiddingEnd ->
      { errorCode: "PlaceBid03"
      , message: "Tx cannot be submitted after bidding end time."
      }
    PlaceBid_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "PlaceBid04"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
    PlaceBid_Error_InvalidAuctionInfo errors ->
      { errorCode: "PlaceBid05"
      , message: "Invalid auction info, errors: " <> show errors <> "."
      }
    PlaceBid_Error_CouldNotFindCurrentStandingBidUtxo ->
      { errorCode: "PlaceBid06"
      , message: "Could not find current standing bid utxo."
      }
