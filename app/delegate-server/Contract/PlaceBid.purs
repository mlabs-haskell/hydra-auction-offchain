module DelegateServer.Contract.PlaceBid
  ( PlaceBidL2ContractError
      ( PlaceBidL2_Error_CurrentTimeBeforeBiddingStart
      , PlaceBidL2_Error_CurrentTimeAfterBiddingEnd
      , PlaceBidL2_Error_CouldNotFindCollateralUtxo
      , PlaceBidL2_Error_CouldNotFindCurrentStandingBidUtxo
      , PlaceBidL2_Error_InvalidBidStateTransition
      , PlaceBidL2_Error_CouldNotBuildAuctionValidators
      )
  , placeBidL2
  , placeBidL2'
  , placeBidL2ContractErrorCodec
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder)
import Contract.BalanceTxConstraints
  ( mustUseAdditionalUtxos
  , mustUseCoinSelectionStrategy
  , mustUseCollateralUtxos
  , mustUseUtxosAtAddresses
  ) as BalancerConstraints
import Contract.Chain (currentTime)
import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (Datum, OutputDatum(NoOutputDatum), Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTimeRange, to)
import Contract.Transaction
  ( FinalizedTransaction
  , Transaction
  , TransactionInput
  , signTransaction
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustNotBeValid
  , mustPayToScript
  , mustSpendScriptOutput
  , mustValidateIn
  ) as Constraints
import Contract.Utxos (UtxoMap)
import Contract.Value (Value)
import Contract.Value (adaSymbol, singleton, symbols) as Value
import Contract.Wallet (getWalletAddress)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, mapExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.BalanceTx.CoinSelection (SelectionStrategy(SelectionStrategyMinimal))
import Data.Array (find) as Array
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Newtype (modify, unwrap)
import Data.Profunctor (dimap)
import Data.Variant (inj, match) as Variant
import DelegateServer.App (runContractNullCosts)
import DelegateServer.Helpers (modifyF)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.Lib.Transaction (setExUnitsToMax, setTxValid)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.State (class AppOpen, access, readAppState)
import DelegateServer.Types.HydraUtxoMap (toUtxoMapWithoutRefScripts)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Contract.MintingPolicies (standingBidTokenName)
import HydraAuctionOffchain.Contract.QueryUtxo (findStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionTerms(AuctionTerms)
  , BidTerms
  , StandingBidRedeemer(NewBidRedeemer)
  , Utxo
  , AuctionInfoRec
  , buildTx
  , emptySubmitTxData
  , validateNewBid
  )
import HydraAuctionOffchain.Contract.Validators
  ( MkAuctionValidatorsError
  , mkAuctionValidators
  , mkAuctionValidatorsErrorCodec
  )
import JS.BigInt (fromInt) as BigInt
import Node.Path (FilePath)
import Type.Proxy (Proxy(Proxy))

placeBidL2
  :: forall m
   . AppOpen m
  => HydraNodeApiWebSocket
  -> BidTerms
  -> ExceptT PlaceBidL2ContractError m Unit
placeBidL2 ws bidTerms = do
  auctionInfo <- readAppState (Proxy :: _ "auctionInfo")
  utxos <-
    readAppState (Proxy :: _ "snapshot")
      <#> toUtxoMapWithoutRefScripts
      <<< _.utxo
      <<< unwrap
  { cardanoSk } <- unwrap <$> access (Proxy :: _ "config")
  mapExceptT runContractNullCosts $
    placeBidL2' (unwrap auctionInfo) bidTerms ws.submitTxL2 utxos cardanoSk

placeBidL2'
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> BidTerms
  -> (Transaction -> Effect Unit)
  -> UtxoMap
  -> FilePath
  -> ExceptT PlaceBidL2ContractError Contract Unit
placeBidL2' auctionInfo bidTerms submitTxL2 utxos cardanoSk = do
  balancedTx <- placeBidL2ContractWithErrors auctionInfo bidTerms utxos
  let validTx = modify setTxValid balancedTx
  evaluatedTx <- lift $ modifyF setExUnitsToMax validTx
  signedTx <- lift $ (withWallet cardanoSk <<< signTransaction) =<< signTransaction
    evaluatedTx
  liftEffect $ submitTxL2 $ unwrap signedTx

placeBidL2ContractWithErrors
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> BidTerms
  -> UtxoMap
  -> ExceptT PlaceBidL2ContractError Contract FinalizedTransaction
placeBidL2ContractWithErrors auctionInfoRec bidTerms utxos = do
  let
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  network <- lift getNetworkId

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError PlaceBidL2_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError PlaceBidL2_Error_CurrentTimeAfterBiddingEnd

  -- Get collateral utxo:
  collateralUtxo <- findCollateralUtxo utxos
    !? PlaceBidL2_Error_CouldNotFindCollateralUtxo

  -- Get current standing bid utxo:
  standingBidUtxo /\ oldBidState <- findStandingBidUtxo auctionInfoRec utxos
    ?? PlaceBidL2_Error_CouldNotFindCurrentStandingBidUtxo

  -- Check bid state transition:
  let newBidState = wrap $ Just bidTerms
  success <- liftEffect $ validateNewBid network auctionCs auctionTerms oldBidState newBidState
  unless success $ throwError PlaceBidL2_Error_InvalidBidStateTransition

  -- Build validators:
  -- TODO: pre-build standing bid validator
  validators <-
    withExceptT PlaceBidL2_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  let
    validatorHashes = unwrap $ validatorHash <$> validators

    txValidRange :: POSIXTimeRange
    txValidRange = to $ auctionTermsRec.biddingEnd - wrap (BigInt.fromInt 1000)

    spendableUtxos :: UtxoMap
    spendableUtxos = Map.fromFoldable [ collateralUtxo, standingBidUtxo ]

    -- StandingBid ---------------------------------------------------

    standingBidOref :: TransactionInput
    standingBidOref = fst standingBidUtxo

    standingBidRedeemer :: Redeemer
    standingBidRedeemer = wrap $ toData NewBidRedeemer

    standingBidDatum :: Datum
    standingBidDatum = wrap $ toData newBidState

    standingBidTokenValue :: Value
    standingBidTokenValue = Value.singleton auctionCs standingBidTokenName one

    --

    balancerConstraints :: BalanceTxConstraintsBuilder
    balancerConstraints = mconcat
      [ BalancerConstraints.mustUseCoinSelectionStrategy SelectionStrategyMinimal
      , BalancerConstraints.mustUseUtxosAtAddresses network []
      , BalancerConstraints.mustUseCollateralUtxos $ Map.fromFoldable [ collateralUtxo ]
      , BalancerConstraints.mustUseAdditionalUtxos spendableUtxos
      ]

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Spend standing bid utxo with old standing bid datum: 
        Constraints.mustSpendScriptOutput standingBidOref standingBidRedeemer

      , -- Lock standing bid token with new standing bid datum at
        -- standing bid validator address:
        Constraints.mustPayToScript validatorHashes.standingBid standingBidDatum DatumInline
          standingBidTokenValue

      , -- Set transaction validity interval to bidding period: 
        Constraints.mustValidateIn txValidRange

      , Constraints.mustNotBeValid
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs spendableUtxos
      , Lookups.validator (unwrap validators).standingBid
      ]

  lift $ buildTx $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    , balancerConstraints = balancerConstraints
    }

----------------------------------------------------------------------
-- Queries

findCollateralUtxo :: UtxoMap -> Contract (Maybe Utxo)
findCollateralUtxo utxos = do
  ownAddress <- liftedM "Could not get wallet address." getWalletAddress
  pure $ Map.toUnfoldable utxos # Array.find
    ( \utxo ->
        let
          txOut = unwrap $ _.output $ unwrap $ snd utxo
        in
          txOut.address == ownAddress
            && (Value.symbols txOut.amount == [ Value.adaSymbol ])
            && (txOut.datum == NoOutputDatum)
            && isNothing txOut.referenceScript
    )

----------------------------------------------------------------------
-- Errors

data PlaceBidL2ContractError
  = PlaceBidL2_Error_CurrentTimeBeforeBiddingStart
  | PlaceBidL2_Error_CurrentTimeAfterBiddingEnd
  | PlaceBidL2_Error_CouldNotFindCollateralUtxo
  | PlaceBidL2_Error_CouldNotFindCurrentStandingBidUtxo
  | PlaceBidL2_Error_InvalidBidStateTransition
  | PlaceBidL2_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError

derive instance Generic PlaceBidL2ContractError _
derive instance Eq PlaceBidL2ContractError

instance Show PlaceBidL2ContractError where
  show = genericShow

instance ToContractError PlaceBidL2ContractError where
  errorCodePrefix = const "PlaceBidL2_"
  errorMessage = case _ of
    PlaceBidL2_Error_CurrentTimeBeforeBiddingStart ->
      "Tx cannot be submitted before bidding start time."

    PlaceBidL2_Error_CurrentTimeAfterBiddingEnd ->
      "Tx cannot be submitted after bidding end time."

    PlaceBidL2_Error_CouldNotFindCollateralUtxo ->
      "Could not find collateral utxo."

    PlaceBidL2_Error_CouldNotFindCurrentStandingBidUtxo ->
      "Could not find current standing bid utxo."

    PlaceBidL2_Error_InvalidBidStateTransition ->
      "Invalid bid state transition."

    PlaceBidL2_Error_CouldNotBuildAuctionValidators err ->
      "Could not build auction validators, error: " <> show err <> "."

placeBidL2ContractErrorCodec :: CA.JsonCodec PlaceBidL2ContractError
placeBidL2ContractErrorCodec =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "CurrentTimeBeforeBiddingStart": Left unit
        , "CurrentTimeAfterBiddingEnd": Left unit
        , "CouldNotFindCollateralUtxo": Left unit
        , "CouldNotFindCurrentStandingBidUtxo": Left unit
        , "InvalidBidStateTransition": Left unit
        , "CouldNotBuildAuctionValidators": Right mkAuctionValidatorsErrorCodec
        }
    )
  where
  toVariant = case _ of
    PlaceBidL2_Error_CurrentTimeBeforeBiddingStart ->
      Variant.inj (Proxy :: _ "CurrentTimeBeforeBiddingStart") unit
    PlaceBidL2_Error_CurrentTimeAfterBiddingEnd ->
      Variant.inj (Proxy :: _ "CurrentTimeAfterBiddingEnd") unit
    PlaceBidL2_Error_CouldNotFindCollateralUtxo ->
      Variant.inj (Proxy :: _ "CouldNotFindCollateralUtxo") unit
    PlaceBidL2_Error_CouldNotFindCurrentStandingBidUtxo ->
      Variant.inj (Proxy :: _ "CouldNotFindCurrentStandingBidUtxo") unit
    PlaceBidL2_Error_InvalidBidStateTransition ->
      Variant.inj (Proxy :: _ "InvalidBidStateTransition") unit
    PlaceBidL2_Error_CouldNotBuildAuctionValidators x ->
      Variant.inj (Proxy :: _ "CouldNotBuildAuctionValidators") x

  fromVariant = Variant.match
    { "CurrentTimeBeforeBiddingStart":
        const PlaceBidL2_Error_CurrentTimeBeforeBiddingStart
    , "CurrentTimeAfterBiddingEnd":
        const PlaceBidL2_Error_CurrentTimeAfterBiddingEnd
    , "CouldNotFindCollateralUtxo":
        const PlaceBidL2_Error_CouldNotFindCollateralUtxo
    , "CouldNotFindCurrentStandingBidUtxo":
        const PlaceBidL2_Error_CouldNotFindCurrentStandingBidUtxo
    , "InvalidBidStateTransition":
        const PlaceBidL2_Error_InvalidBidStateTransition
    , "CouldNotBuildAuctionValidators":
        PlaceBidL2_Error_CouldNotBuildAuctionValidators
    }

