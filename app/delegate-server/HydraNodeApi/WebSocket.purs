module DelegateServer.HydraNodeApi.WebSocket
  ( HydraNodeApiWebSocket
  , mkHydraNodeApiWebSocket
  ) where

import Prelude

import Contract.Log (logInfo', logWarn')
import Contract.Transaction (Transaction)
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
import Data.Tuple.Nested ((/\))
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
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus
      ( HeadStatus_Initializing
      , HeadStatus_Open
      , HeadStatus_Closed
      , HeadStatus_FanoutPossible
      , HeadStatus_Final
      )
  , printHeadStatus
  )
import DelegateServer.Types.HydraNodeApiMessage
  ( CommittedMessage
  , GreetingsMessage
  , HeadClosedMessage
  , HeadFinalizedMessage
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
  , HydraNodeApi_OutMessage(Out_Init, Out_Abort, Out_NewTx, Out_Close, Out_Contest, Out_Fanout)
  , PeerConnMessage
  , SnapshotConfirmedMessage
  , HeadInitMessage
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  )
import DelegateServer.Types.HydraSnapshot (HydraSnapshot, hydraSnapshotCodec)
import DelegateServer.Types.HydraTx (mkHydraTx)
import DelegateServer.WebSocket (WebSocket, mkWebSocket)
import DelegateServer.WsServer
  ( DelegateWebSocketServer
  , DelegateWebSocketServerMessage(HydraHeadStatus, StandingBid)
  )
import Effect (Effect)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Lib.Json (printJsonUsingCodec)
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(Proxy))

type HydraNodeApiWebSocket =
  { baseWs :: WebSocket AppM HydraNodeApi_InMessage HydraNodeApi_OutMessage
  , initHead :: Effect Unit
  , abortHead :: Effect Unit
  , submitTxL2 :: Transaction -> Effect Unit
  , closeHead :: Effect Unit
  , challengeSnapshot :: Effect Unit
  , fanout :: Effect Unit
  }

mkHydraNodeApiWebSocket
  :: DelegateWebSocketServer -> (HydraNodeApiWebSocket -> AppM Unit) -> AppM Unit
mkHydraNodeApiWebSocket wsServer onConnect = do
  config <- unwrap <$> asks _.config
  runM <- getAppEffRunner
  liftEffect do
    ws /\ wsUrl <- mkWebSocket
      { hostPort: config.hydraNodeApi
      , inMsgCodec: hydraNodeApiInMessageCodec
      , outMsgCodec: hydraNodeApiOutMessageCodec
      , runM
      }
    let
      hydraNodeApiWs :: HydraNodeApiWebSocket
      hydraNodeApiWs =
        { baseWs: ws
        , initHead: ws.send Out_Init
        , abortHead: ws.send Out_Abort
        , submitTxL2: ws.send <<< Out_NewTx <<< { transaction: _ } <=< mkHydraTx
        , closeHead: ws.send Out_Close
        , challengeSnapshot: ws.send Out_Contest
        , fanout: ws.send Out_Fanout
        }
    ws.onConnect $ connectHandler wsUrl *> onConnect hydraNodeApiWs
    ws.onMessage (messageHandler hydraNodeApiWs wsServer)
    ws.onError errorHandler

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
  => HydraNodeApiWebSocket
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
  { config: AppConfig config, livePeers: livePeersAVar } <- accessRec
    (Proxy :: _ ("config" :> "livePeers" :> Nil'))
  modifyAVar_ livePeersAVar \livePeers ->
    case Set.member peer livePeers, peer /= config.hydraNodeId of
      false, true -> do
        let livePeers' = Set.insert peer livePeers
        logInfo' $ "Peer connected (live peers " <> show (Set.size livePeers') <> "/"
          <> show (Array.length config.peers)
          <> ")."
        pure livePeers'
      _, _ -> pure livePeers

msgPeerDisconnectedHandler :: forall m. AppBase m => PeerConnMessage -> m Unit
msgPeerDisconnectedHandler { peer } = do
  { config: AppConfig config, livePeers: livePeersAVar } <- accessRec
    (Proxy :: _ ("config" :> "livePeers" :> Nil'))
  modifyAVar_ livePeersAVar \livePeers ->
    case Set.member peer livePeers, peer /= config.hydraNodeId of
      true, true -> do
        let livePeers' = Set.delete peer livePeers
        logInfo' $ "Peer disconnected (live peers " <> show (Set.size livePeers')
          <> "/"
          <> show (Array.length config.peers)
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
  -> HydraNodeApiWebSocket
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
  -> HydraNodeApiWebSocket
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
  liftEffect $ wsServer.broadcast (HydraHeadStatus status)

setSnapshot' :: forall m. AppOpen m => DelegateWebSocketServer -> HydraSnapshot -> m Unit
setSnapshot' wsServer snapshot = do
  setSnapshot snapshot
  logInfo' $ "New confirmed snapshot: " <> printJsonUsingCodec hydraSnapshotCodec
    snapshot
  standingBid <- map snd <$> queryStandingBidL2
  liftEffect $ traverse_ (wsServer.broadcast <<< StandingBid)
    standingBid
