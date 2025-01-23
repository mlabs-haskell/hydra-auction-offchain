module DelegateServer.HydraNodeApi.WebSocket
  ( mkHydraNodeApiWebSocket
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Contract.CborBytes (hexToCborBytes)
import Contract.Log (logInfo', logWarn')
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader (asks)
import Data.Array (length) as Array
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (delete, insert, member, size) as Set
import Data.Traversable (traverse_)
import Data.Tuple (snd)
import DelegateServer.App (AppM, getAppEffRunner)
import DelegateServer.Config (AppConfig'(AppConfig))
import DelegateServer.Contract.Commit (commitCollateral, commitStandingBid)
import DelegateServer.Contract.StandingBid (queryStandingBidL2)
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.State
  ( class App
  , class AppBase
  , class AppInit
  , class AppOpen
  , accessRec
  , exitWithReason
  , readAppState
  , setCommitStatus
  , setHeadCs
  , setHeadStatus
  , setSnapshot
  )
import DelegateServer.Types.AppExitReason (AppExitReason(AppExitReason_HeadFinalized))
import DelegateServer.Types.CommitStatus
  ( CommitStatus(ShouldCommitCollateral, ShouldCommitStandingBid)
  )
import DelegateServer.WsServer
  ( DelegateWebSocketServer
  , DelegateWebSocketServerMessage(HydraHeadStatus, StandingBid)
  )
import Effect.Class (liftEffect)
import Effect.Exception (error)
import HydraAuctionOffchain.Lib.Json (printJsonUsingCodec)
import HydraSdk.NodeApi
  ( HydraNodeApiWebSocket
  , HydraTxRetryStrategy(RetryTxWithParams, DontRetryTx)
  )
import HydraSdk.NodeApi (mkHydraNodeApiWebSocket) as HydraSdk
import HydraSdk.Types
  ( CommittedMessage
  , HeadClosedMessage
  , HeadFinalizedMessage
  , HeadInitMessage
  , HeadOpenMessage
  , HydraNodeApi_InMessage
      ( Greetings
      , PeerConnected
      , PeerDisconnected
      , HeadIsInitializing
      , Committed
      , HeadIsAborted
      , HeadIsOpen
      , SnapshotConfirmed
      , TxInvalid
      , HeadIsClosed
      , ReadyToFanout
      , HeadIsFinalized
      )
  , HydraHeadStatus(HeadStatus_Closed)
  , HydraSnapshot
  , PeerConnMessage
  , SnapshotConfirmedMessage
  , GreetingsMessage
  , hydraSnapshotCodec
  , printHeadStatus
  , printHostPort
  )
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(Proxy))

mkHydraNodeApiWebSocket
  :: DelegateWebSocketServer
  -> (HydraNodeApiWebSocket AppM -> AppM Unit)
  -> AppM Unit
mkHydraNodeApiWebSocket wsServer onConnect = do
  runM <- getAppEffRunner
  { auctionConfig: { hydraNodeApi } } <- unwrap <$> asks _.config
  let url = "ws://" <> printHostPort hydraNodeApi
  void $ HydraSdk.mkHydraNodeApiWebSocket
    { url
    , runM
    , handlers:
        { connectHandler: \ws -> connectHandler url *> onConnect ws
        , messageHandler: \ws message -> messageHandler ws wsServer message
        , errorHandler: const errorHandler
        , headStatusHandler: Just (setHeadStatus' wsServer)
        }
    , txRetryStrategies:
        { close:
            RetryTxWithParams
              { delaySec: 90
              , maxRetries: top
              , successPredicate:
                  readAppState (Proxy :: _ "headStatus") <#> \headStatus ->
                    headStatus >= HeadStatus_Closed
              , failHandler: pure unit
              }
        , contest: DontRetryTx
        }
    }

----------------------------------------------------------------------
-- Handlers

connectHandler :: forall m. MonadLogger m => String -> m Unit
connectHandler wsUrl = logInfo' $ "Connected to hydra-node-api ws server (" <> wsUrl <> ")."

errorHandler :: forall m. MonadLogger m => String -> m Unit
errorHandler = logInfo' <<< append "hydra-node-api ws error: "

--

messageHandler
  :: forall m
   . App m
  => HydraNodeApiWebSocket m
  -> DelegateWebSocketServer
  -> Either String HydraNodeApi_InMessage
  -> m Unit
messageHandler ws wsServer = case _ of
  Left _rawMessage -> pure unit
  Right parsedMessage ->
    case parsedMessage of
      Greetings msg -> msgGreetingsHandler wsServer msg
      PeerConnected msg -> msgPeerConnectedHandler msg
      PeerDisconnected msg -> msgPeerDisconnectedHandler msg
      HeadIsInitializing msg -> msgHeadIsInitializingHandler msg
      Committed msg -> msgCommittedHandler msg
      HeadIsAborted _ -> msgHeadAbortedHandler wsServer
      HeadIsOpen msg -> msgHeadOpenHandler wsServer msg
      SnapshotConfirmed msg -> msgSnapshotConfirmedHandler wsServer msg
      TxInvalid _ -> pure unit
      HeadIsClosed msg -> msgHeadClosedHandler ws msg
      ReadyToFanout _ -> msgReadyToFanoutHandler ws
      HeadIsFinalized msg -> msgHeadFinalizedHandler msg
      _ -> pure unit

msgGreetingsHandler
  :: forall m
   . AppOpen m
  => DelegateWebSocketServer
  -> GreetingsMessage
  -> m Unit
msgGreetingsHandler wsServer { snapshotUtxo } =
  setSnapshot' wsServer $ wrap
    { snapshotNumber: zero -- FIXME: hydra-node: `Greetings` message should include snapshot number.
    , utxo: fromMaybe mempty snapshotUtxo
    , confirmedTransactions: mempty
    }

msgPeerConnectedHandler :: forall m. AppBase m => PeerConnMessage -> m Unit
msgPeerConnectedHandler { peer } = do
  { config: AppConfig { auctionConfig }, livePeers: livePeersAVar } <- accessRec
    (Proxy :: _ ("config" :> "livePeers" :> Nil'))
  modifyAVar_ livePeersAVar \livePeers ->
    case Set.member peer livePeers, peer /= auctionConfig.hydraNodeId of
      false, true -> do
        let livePeers' = Set.insert peer livePeers
        logInfo' $ "Peer connected (live peers " <> show (Set.size livePeers') <> "/"
          <> show (Array.length auctionConfig.peers)
          <> ")."
        pure livePeers'
      _, _ -> pure livePeers

msgPeerDisconnectedHandler :: forall m. AppBase m => PeerConnMessage -> m Unit
msgPeerDisconnectedHandler { peer } = do
  { config: AppConfig { auctionConfig }, livePeers: livePeersAVar } <- accessRec
    (Proxy :: _ ("config" :> "livePeers" :> Nil'))
  modifyAVar_ livePeersAVar \livePeers ->
    case Set.member peer livePeers, peer /= auctionConfig.hydraNodeId of
      true, true -> do
        let livePeers' = Set.delete peer livePeers
        logInfo' $ "Peer disconnected (live peers " <> show (Set.size livePeers')
          <> "/"
          <> show (Array.length auctionConfig.peers)
          <> ")."
        pure livePeers'
      _, _ -> pure livePeers

msgHeadIsInitializingHandler :: forall m. AppInit m => HeadInitMessage -> m Unit
msgHeadIsInitializingHandler { headId } = do
  headCs <-
    liftMaybe (error "msgHeadIsInitializingHandler: could not decode Head currency symbol")
      (decodeCbor =<< hexToCborBytes headId)
  setHeadCs headCs
  commitStatus <- readAppState (Proxy :: _ "commitStatus")
  when (commitStatus == ShouldCommitStandingBid) do
    runExceptT commitStandingBid >>=
      either
        ( \err ->
            logWarn' ("Could not commit standing bid, error: " <> show err <> ".")
              *> setCommitStatus ShouldCommitCollateral
        )
        (const (pure unit))

msgCommittedHandler :: forall m. AppInit m => CommittedMessage -> m Unit
msgCommittedHandler _ =
  runExceptT commitCollateral >>=
    either
      (\err -> logWarn' $ "Could not commit collateral, error: " <> show err <> ".")
      (const (pure unit))

msgHeadAbortedHandler :: forall m. AppInit m => DelegateWebSocketServer -> m Unit
msgHeadAbortedHandler _ =
  exitWithReason AppExitReason_HeadFinalized

msgHeadOpenHandler
  :: forall m
   . AppOpen m
  => DelegateWebSocketServer
  -> HeadOpenMessage
  -> m Unit
msgHeadOpenHandler wsServer { utxo } =
  setSnapshot' wsServer $ wrap
    { snapshotNumber: zero
    , utxo
    , confirmedTransactions: mempty
    }

msgSnapshotConfirmedHandler
  :: forall m
   . AppOpen m
  => DelegateWebSocketServer
  -> SnapshotConfirmedMessage
  -> m Unit
msgSnapshotConfirmedHandler wsServer =
  setSnapshot' wsServer <<< _.snapshot

msgHeadClosedHandler
  :: forall m
   . AppOpen m
  => HydraNodeApiWebSocket m
  -> HeadClosedMessage
  -> m Unit
msgHeadClosedHandler ws { snapshotNumber } = do
  ownSnapshot <- unwrap <$> readAppState (Proxy :: _ "snapshot")
  when (ownSnapshot.snapshotNumber > snapshotNumber) $
    liftEffect ws.challengeSnapshot

msgReadyToFanoutHandler :: forall m. AppBase m => HydraNodeApiWebSocket m -> m Unit
msgReadyToFanoutHandler ws =
  liftEffect ws.fanout

msgHeadFinalizedHandler :: forall m. AppBase m => HeadFinalizedMessage -> m Unit
msgHeadFinalizedHandler _ =
  exitWithReason AppExitReason_HeadFinalized

--

setHeadStatus' :: forall m. AppBase m => DelegateWebSocketServer -> HydraHeadStatus -> m Unit
setHeadStatus' wsServer status = do
  setHeadStatus status
  logInfo' $ "New head status: " <> printHeadStatus status <> "."
  auctionCs <- _.auctionId <<< unwrap <$> readAppState (Proxy :: _ "auctionInfo")
  liftEffect $ wsServer.broadcast auctionCs (HydraHeadStatus status)

setSnapshot' :: forall m. AppOpen m => DelegateWebSocketServer -> HydraSnapshot -> m Unit
setSnapshot' wsServer snapshot = do
  setSnapshot snapshot
  logInfo' $ "New confirmed snapshot: " <> printJsonUsingCodec hydraSnapshotCodec
    snapshot
  standingBid <- map snd <$> queryStandingBidL2
  auctionCs <- _.auctionId <<< unwrap <$> readAppState (Proxy :: _ "auctionInfo")
  liftEffect $ traverse_ (wsServer.broadcast auctionCs <<< StandingBid)
    standingBid
