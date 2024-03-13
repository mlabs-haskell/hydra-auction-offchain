module DelegateServer.Contract.PlaceBid
  ( placeBidL2
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
import Contract.Transaction (FinalizedTransaction, TransactionInput, signTransaction)
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
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.BalanceTx.CoinSelection (SelectionStrategy(SelectionStrategyMinimal))
import Data.Array (find) as Array
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Newtype (modify)
import DelegateServer.Helpers (modifyF)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.Lib.Transaction (setExUnitsToMax, setTxValid)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.State (AppM, readAppState, runContract, runContractNullCosts)
import DelegateServer.Types.HydraUtxoMap (toUtxoMapWithoutRefScripts)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Contract.MintingPolicies (standingBidTokenName)
import HydraAuctionOffchain.Contract.QueryUtxo (findStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfoExtended
  , AuctionTerms(AuctionTerms)
  , BidTerms
  , ContractOutput(ContractOutputError, ContractOutputResult)
  , StandingBidRedeemer(NewBidRedeemer)
  , Utxo
  , buildTx
  , contractErrorCodec
  , emptySubmitTxData
  , mkContractOutput
  , validateNewBid
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Lib.Json (caEncodeString)
import JS.BigInt (fromInt) as BigInt

placeBidL2 :: HydraNodeApiWebSocket -> BidTerms -> AppM Unit
placeBidL2 ws bidTerms = do
  auctionInfo <- readAppState _.auctionInfo
  utxos <- readAppState _.snapshot <#> toUtxoMapWithoutRefScripts <<< _.utxo
  res <- runContractNullCosts do
    mkContractOutput identity $
      placeBidL2ContractWithErrors auctionInfo bidTerms utxos
  case res of
    ContractOutputError err ->
      liftEffect $ log $ "Failed to place bid on L2: " <> caEncodeString contractErrorCodec err
    ContractOutputResult balancedTx -> do
      let validTx = modify setTxValid balancedTx
      evaluatedTx <- runContract $ modifyF setExUnitsToMax validTx
      { cardanoSk } <- asks _.config
      signedTx <- runContract do
        signedTx' <- signTransaction evaluatedTx
        withWallet cardanoSk $ signTransaction signedTx'
      liftEffect $ ws.newTx $ unwrap signedTx

placeBidL2ContractWithErrors
  :: AuctionInfoExtended
  -> BidTerms
  -> UtxoMap
  -> ExceptT PlaceBidL2ContractError Contract FinalizedTransaction
placeBidL2ContractWithErrors auctionInfo bidTerms utxos = do
  let
    auctionInfoRec = unwrap auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  networkId <- lift getNetworkId

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
  success <- liftEffect $ validateNewBid auctionCs auctionTerms oldBidState newBidState
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
      , BalancerConstraints.mustUseUtxosAtAddresses networkId []
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
  toContractError = wrap <<< case _ of
    PlaceBidL2_Error_CurrentTimeBeforeBiddingStart ->
      { errorCode: "PlaceBidL201"
      , message: "Tx cannot be submitted before bidding start time."
      }
    PlaceBidL2_Error_CurrentTimeAfterBiddingEnd ->
      { errorCode: "PlaceBidL202"
      , message: "Tx cannot be submitted after bidding end time."
      }
    PlaceBidL2_Error_CouldNotFindCollateralUtxo ->
      { errorCode: "PlaceBidL203"
      , message: "Could not find collateral utxo."
      }
    PlaceBidL2_Error_CouldNotFindCurrentStandingBidUtxo ->
      { errorCode: "PlaceBidL204"
      , message: "Could not find current standing bid utxo."
      }
    PlaceBidL2_Error_InvalidBidStateTransition ->
      { errorCode: "PlaceBidL205"
      , message: "Invalid bid state transition."
      }
    PlaceBidL2_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "PlaceBidL206"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
