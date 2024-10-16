module DelegateServer.HydraNodeApi.WebSocket
  ( mkHydraNodeApiWebSocket
  ) where

import Prelude

import Contract.Log (logInfo', logWarn')
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader (asks)
import Data.Array (length) as Array
import Data.Either (either)
import Data.Maybe (fromMaybe)
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
      ( In_Greetings
      , In_PeerConnected
      , In_PeerDisconnected
      , In_HeadIsInitializing
      , In_Committed
      , In_HeadIsAborted
      , In_HeadIsOpen
      , In_SnapshotConfirmed
      , In_TxInvalid
      , In_HeadIsClosed
      , In_ReadyToFanout
      , In_HeadIsFinalized
      )
  , HydraHeadStatus
      ( HeadStatus_Initializing
      , HeadStatus_Open
      , HeadStatus_Closed
      , HeadStatus_FanoutPossible
      , HeadStatus_Final
      )
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
  -> HydraNodeApi_InMessage
  -> m Unit
messageHandler ws wsServer = case _ of
  In_Greetings msg -> msgGreetingsHandler wsServer msg
  In_PeerConnected msg -> msgPeerConnectedHandler msg
  In_PeerDisconnected msg -> msgPeerDisconnectedHandler msg
  In_HeadIsInitializing msg -> msgHeadIsInitializingHandler wsServer msg
  In_Committed msg -> msgCommittedHandler msg
  In_HeadIsAborted -> msgHeadAbortedHandler wsServer
  In_HeadIsOpen msg -> msgHeadOpenHandler wsServer msg
  In_SnapshotConfirmed msg -> msgSnapshotConfirmedHandler wsServer msg
  In_TxInvalid -> pure unit
  In_HeadIsClosed msg -> msgHeadClosedHandler wsServer ws msg
  In_ReadyToFanout -> msgReadyToFanoutHandler wsServer ws
  In_HeadIsFinalized msg -> msgHeadFinalizedHandler wsServer msg

msgGreetingsHandler
  :: forall m
   . AppOpen m
  => DelegateWebSocketServer
  -> GreetingsMessage
  -> m Unit
msgGreetingsHandler wsServer { headStatus, snapshotUtxo } = do
  setHeadStatus' wsServer headStatus
  setSnapshot' wsServer $ wrap
    { snapshotNumber: zero -- FIXME: hydra-node: `Greetings` message should include snapshot number.
    , utxo: fromMaybe mempty snapshotUtxo
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

msgHeadIsInitializingHandler
  :: forall m
   . AppInit m
  => DelegateWebSocketServer
  -> HeadInitMessage
  -> m Unit
msgHeadIsInitializingHandler wsServer { headId } = do
  setHeadCs headId
  setHeadStatus' wsServer HeadStatus_Initializing
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
msgHeadAbortedHandler wsServer = do
  setHeadStatus' wsServer HeadStatus_Final
  exitWithReason AppExitReason_HeadFinalized

msgHeadOpenHandler
  :: forall m
   . AppOpen m
  => DelegateWebSocketServer
  -> HeadOpenMessage
  -> m Unit
msgHeadOpenHandler wsServer { utxo } = do
  setHeadStatus' wsServer HeadStatus_Open
  setSnapshot' wsServer $ wrap
    { snapshotNumber: zero
    , utxo
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
  => DelegateWebSocketServer
  -> HydraNodeApiWebSocket m
  -> HeadClosedMessage
  -> m Unit
msgHeadClosedHandler wsServer ws { snapshotNumber } = do
  setHeadStatus' wsServer HeadStatus_Closed
  ownSnapshot <- unwrap <$> readAppState (Proxy :: _ "snapshot")
  when (ownSnapshot.snapshotNumber > snapshotNumber) $
    liftEffect ws.challengeSnapshot

msgReadyToFanoutHandler
  :: forall m
   . AppBase m
  => DelegateWebSocketServer
  -> HydraNodeApiWebSocket m
  -> m Unit
msgReadyToFanoutHandler wsServer ws = do
  setHeadStatus' wsServer HeadStatus_FanoutPossible
  liftEffect $ ws.fanout

msgHeadFinalizedHandler
  :: forall m
   . AppBase m
  => DelegateWebSocketServer
  -> HeadFinalizedMessage
  -> m Unit
msgHeadFinalizedHandler wsServer _ = do
  setHeadStatus' wsServer HeadStatus_Final
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
