module DelegateServer.Contract.Commit
  ( CommitCollateralError
      ( CommitCollateral_Error_CommitRequestFailed
      , CommitCollateral_Error_SubmitTxFailed
      )
  , CommitStandingBidError
      ( CommitBid_Error_CurrentTimeBeforeBiddingStart
      , CommitBid_Error_CurrentTimeAfterBiddingEnd
      , CommitBid_Error_CouldNotFindStandingBidUtxo
      , CommitBid_Error_CouldNotIndexRedeemers
      , CommitBid_Error_CommitRequestFailed
      , CommitBid_Error_CommitMultiSignFailed
      , CommitBid_Error_SubmitTxFailed
      )
  , commitCollateral
  , commitStandingBid
  , commitStandingBidErrorCodec
  ) where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Log (logDebug', logWarn')
import Contract.Monad (Contract)
import Contract.PlutusData (Redeemer, toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Time (POSIXTimeRange, mkFiniteInterval)
import Contract.Transaction
  ( BalancedSignedTransaction
  , Transaction
  , TransactionHash
  , TransactionInput
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  , mustValidateIn
  ) as Constraints
import Contract.UnbalancedTx (UnbalancedTx(UnbalancedTx), mkUnbalancedTx)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT(ExceptT), mapExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.BalanceTx.RedeemerIndex
  ( attachIndexedRedeemers
  , indexRedeemers
  , mkRedeemersContext
  )
import Data.Argonaut (Json, encodeJson)
import Data.Codec.Argonaut (JsonCodec, encode) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Map (fromFoldable) as Map
import Data.Show.Generic (genericShow)
import DelegateServer.App (runContract, runContractLift)
import DelegateServer.Handlers.SignCommitTx (signCommitTxErrorCodec)
import DelegateServer.Helpers (modifyF)
import DelegateServer.HydraNodeApi.Http (commit)
import DelegateServer.Lib.ServerConfig (mkLocalhostHttpServerConfig)
import DelegateServer.Lib.Transaction (appendTxSignatures, reSignTransaction, setAuxDataHash)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.PeerDelegate.Http (signCommitTx)
import DelegateServer.State (class AppBase, class AppInit, access, readAppState)
import DelegateServer.Types.HydraCommitRequest (mkFullCommitRequest, mkSimpleCommitRequest)
import DelegateServer.Types.HydraHeadPeer (HydraHeadPeer)
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  )
import HydraAuctionOffchain.Contract.QueryUtxo (queryStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoRec
  , AuctionTerms(AuctionTerms)
  , StandingBidRedeemer(MoveToHydraRedeemer)
  , StandingBidState
  , Utxo
  )
import HydraAuctionOffchain.Contract.Validators (mkStandingBidValidator)
import HydraAuctionOffchain.Helpers ((!*))
import HydraAuctionOffchain.Lib.Json (printJson)
import HydraAuctionOffchain.Service.Common (ServiceError)
import JS.BigInt (fromInt) as BigInt
import Type.Proxy (Proxy(Proxy))

buildCommitTx
  :: forall m. AppBase m => Json -> ExceptT ServiceError m BalancedSignedTransaction
buildCommitTx commitRequest = do
  { hydraNodeApi, cardanoSk } <- unwrap <$> access (Proxy :: _ "config")
  let serverConfig = mkLocalhostHttpServerConfig hydraNodeApi.port
  draftCommitTx <- ExceptT $ liftAff $ commit serverConfig commitRequest
  runContractLift do
    -- FIXME: auxiliary data hash set by hydra-node seems to be
    -- invalid, so we recompute it here
    commitTx <- wrap <$> setAuxDataHash draftCommitTx.cborHex
    signedTx <-
      (withWallet cardanoSk <<< signTransaction)
        =<< reSignTransaction commitTx
    pure signedTx

----------------------------------------------------------------------
-- Commit only collateral

data CommitCollateralError
  = CommitCollateral_Error_CommitRequestFailed ServiceError
  | CommitCollateral_Error_SubmitTxFailed

derive instance Generic CommitCollateralError _

instance Show CommitCollateralError where
  show = genericShow

commitCollateral :: forall m. AppInit m => ExceptT CommitCollateralError m TransactionHash
commitCollateral = do
  collateralUtxo <- readAppState (Proxy :: _ "collateralUtxo")
  let
    utxos = Map.fromFoldable [ collateralUtxo ]
    commitRequest = encodeJson $ mkSimpleCommitRequest utxos
  logDebug' $ "Collateral commit request: " <> printJson commitRequest
  commitTx <-
    withExceptT CommitCollateral_Error_CommitRequestFailed $
      buildCommitTx commitRequest
  txHash <- runContract (submit commitTx) !* CommitCollateral_Error_SubmitTxFailed
  pure txHash

----------------------------------------------------------------------
-- Commit standing bid and collateral

data CommitStandingBidError
  = CommitBid_Error_CurrentTimeBeforeBiddingStart
  | CommitBid_Error_CurrentTimeAfterBiddingEnd
  | CommitBid_Error_CouldNotFindStandingBidUtxo
  | CommitBid_Error_CouldNotIndexRedeemers
  | CommitBid_Error_CommitRequestFailed
  | CommitBid_Error_CommitMultiSignFailed
  | CommitBid_Error_SubmitTxFailed

derive instance Generic CommitStandingBidError _
derive instance Eq CommitStandingBidError

instance Show CommitStandingBidError where
  show = genericShow

commitStandingBidErrorCodec :: CA.JsonCodec CommitStandingBidError
commitStandingBidErrorCodec =
  CAG.nullarySum "CommitStandingBidError"

commitStandingBid
  :: forall m
   . AppInit m
  => ExceptT CommitStandingBidError m
       (StandingBidState /\ TransactionHash)
commitStandingBid = do
  auctionInfo <- unwrap <$> readAppState (Proxy :: _ "auctionInfo")
  collateralUtxo <- readAppState (Proxy :: _ "collateralUtxo")
  { blueprintTx, standingBid, standingBidUtxo } <-
    mapExceptT runContract $
      moveToHydraUnbalancedTx auctionInfo collateralUtxo
  let utxos = Map.fromFoldable [ standingBidUtxo, collateralUtxo ]
  commitRequest <- liftEffect $ encodeJson <$> mkFullCommitRequest blueprintTx utxos
  logDebug' $ "Standing bid commit request: " <> printJson commitRequest
  commitTx <-
    withExceptT (const CommitBid_Error_CommitRequestFailed) $
      buildCommitTx commitRequest
  { peers } <- unwrap <$> access (Proxy :: _ "config")
  commitTxMultiSigned <- multiSignCommitTx peers `modifyF` commitTx
  txHash <- runContract (submit commitTxMultiSigned) !* CommitBid_Error_SubmitTxFailed
  pure $ standingBid /\ txHash

-- Gather signatures of all delegates.
multiSignCommitTx
  :: forall m
   . AppBase m
  => Array HydraHeadPeer
  -> Transaction
  -> ExceptT CommitStandingBidError m Transaction
multiSignCommitTx peers commitTx = do
  responses <-
    withExceptT (const CommitBid_Error_CommitRequestFailed) $
      -- TODO: use parTraverse
      traverse (ExceptT <<< liftAff <<< flip signCommitTx commitTx <<< _.httpServer)
        peers
  signatures <-
    traverse
      ( case _ of
          ServerResponseSuccess signatures ->
            pure signatures
          ServerResponseError signCommitTxErr -> do
            let err = printJson $ CA.encode signCommitTxErrorCodec signCommitTxErr
            logWarn' $ "SignCommitTx request failed, error: " <> err
            throwError CommitBid_Error_CommitMultiSignFailed
      )
      responses
  pure $ appendTxSignatures signatures commitTx

moveToHydraUnbalancedTx
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> Utxo
  -> ExceptT CommitStandingBidError Contract
       { blueprintTx :: Transaction
       , standingBid :: StandingBidState
       , standingBidUtxo :: Utxo
       }
moveToHydraUnbalancedTx auctionInfo collateralUtxo = do
  let
    auctionCs = auctionInfo.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfo.auctionTerms

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError CommitBid_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError CommitBid_Error_CurrentTimeAfterBiddingEnd

  -- Query standing bid utxo:
  standingBidUtxo /\ standingBid <- queryStandingBidUtxo auctionInfo
    !? CommitBid_Error_CouldNotFindStandingBidUtxo

  -- Build standing bid validator:
  -- TODO: pre-build validator or use reference script 
  standingBidValidator <- lift $ mkStandingBidValidator auctionCs auctionTerms

  let
    -- StandingBid ---------------------------------------------------

    standingBidOref :: TransactionInput
    standingBidOref = fst standingBidUtxo

    standingBidRedeemer :: Redeemer
    standingBidRedeemer = wrap $ toData MoveToHydraRedeemer

    -- Collateral ----------------------------------------------------

    collateralOref :: TransactionInput
    collateralOref = fst collateralUtxo

    --

    txValidRange :: POSIXTimeRange
    txValidRange =
      mkFiniteInterval nowTime
        (auctionTermsRec.biddingEnd - wrap (BigInt.fromInt 1000))

    constraints :: TxConstraints
    constraints = mconcat
      [ -- Spend standing bid utxo:
        Constraints.mustSpendScriptOutput standingBidOref standingBidRedeemer

      , -- Spend collateral utxo:
        Constraints.mustSpendPubKeyOutput collateralOref

      , -- Set transaction validity interval to bidding period: 
        Constraints.mustValidateIn txValidRange

      , -- Require signatures of all delegates listed in auction terms:
        foldMap (Constraints.mustBeSignedBy <<< wrap)
          auctionTermsRec.delegates
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.fromFoldable [ standingBidUtxo, collateralUtxo ]
      , Lookups.validator standingBidValidator
      ]

  UnbalancedTx { transaction, redeemers } <- lift $ mkUnbalancedTx lookups constraints
  indexedRedeemers <-
    hush (indexRedeemers (mkRedeemersContext transaction) redeemers)
      ?? CommitBid_Error_CouldNotIndexRedeemers
  pure
    { blueprintTx: attachIndexedRedeemers indexedRedeemers transaction
    , standingBid
    , standingBidUtxo
    }
