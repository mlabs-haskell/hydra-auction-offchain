module DelegateServer.HydraNodeApi.WebSocket
  ( HydraNodeApiWebSocket
  , mkHydraNodeApiWebSocket
  ) where

import Prelude

import Contract.Transaction (Transaction)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import Data.Array (length) as Array
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (delete, insert, member, size) as Set
import Data.Traversable (traverse_)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import DelegateServer.App (AppM, runAppEff)
import DelegateServer.Config (AppConfig(AppConfig))
import DelegateServer.Contract.Commit (commitCollateral, commitStandingBid)
import DelegateServer.Contract.StandingBid (queryStandingBidL2)
import DelegateServer.HydraNodeApi.Types.Message
  ( CommittedMessage
  , GreetingsMessage
  , HeadClosedMessage
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
  , HeadFinalizedMessage
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  )
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
import DelegateServer.Types.HydraSnapshot (HydraSnapshot, hydraSnapshotCodec)
import DelegateServer.WebSocket (WebSocket, mkWebSocket)
import DelegateServer.WsServer (DelegateWebSocketServer)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
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
mkHydraNodeApiWebSocket wsServer onConnect =
  ask >>= \appState -> liftEffect do
    ws /\ wsUrl <- mkWebSocket
      { hostPort: (unwrap appState.config).hydraNodeApi
      , inMsgCodec: hydraNodeApiInMessageCodec
      , outMsgCodec: hydraNodeApiOutMessageCodec
      , runM: runAppEff appState
      }
    let
      hydraNodeApiWs :: HydraNodeApiWebSocket
      hydraNodeApiWs =
        { baseWs: ws
        , initHead: ws.send Out_Init
        , abortHead: ws.send Out_Abort
        , submitTxL2: ws.send <<< Out_NewTx <<< { transaction: _ }
        , closeHead: ws.send Out_Close
        , challengeSnapshot: ws.send Out_Contest
        , fanout: ws.send Out_Fanout
        }
    ws.onConnect $ liftEffect (connectHandler wsUrl) *> onConnect hydraNodeApiWs
    ws.onMessage (messageHandler hydraNodeApiWs wsServer)
    ws.onError (liftEffect <<< errorHandler)

----------------------------------------------------------------------
-- Handlers

connectHandler :: String -> Effect Unit
connectHandler wsUrl = log $ "Connected to hydra-node-api ws server (" <> wsUrl <> ")."

errorHandler :: String -> Effect Unit
errorHandler = log <<< append "hydra-node-api ws error: "

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
  In_HeadIsInitializing -> msgHeadIsInitializingHandler
  In_Committed msg -> msgCommittedHandler msg
  In_HeadIsAborted -> msgHeadAbortedHandler
  In_HeadIsOpen msg -> msgHeadOpenHandler wsServer msg
  In_SnapshotConfirmed msg -> msgSnapshotConfirmedHandler wsServer msg
  In_TxInvalid -> pure unit
  In_HeadIsClosed msg -> msgHeadClosedHandler ws msg
  In_ReadyToFanout -> msgReadyToFanoutHandler ws
  In_HeadIsFinalized msg -> msgHeadFinalizedHandler msg

msgGreetingsHandler
  :: forall m
   . AppOpen m
  => DelegateWebSocketServer
  -> GreetingsMessage
  -> m Unit
msgGreetingsHandler wsServer { headStatus, snapshotUtxo } = do
  setHeadStatus' headStatus
  setSnapshot' wsServer $ wrap
    { snapshotNumber: zero -- FIXME: hydra-node: `Greetings` message should include snapshot number.
    , utxo: fromMaybe mempty snapshotUtxo
    }

msgPeerConnectedHandler :: forall m. AppBase m => PeerConnMessage -> m Unit
msgPeerConnectedHandler { peer } = do
  { config: AppConfig config, livePeers: livePeersAVar } <- accessRec
    (Proxy :: _ ("config" :> "livePeers" :> Nil'))
  liftAff $ modifyAVar_ livePeersAVar \livePeers ->
    case Set.member peer livePeers, peer /= config.hydraNodeId of
      false, true -> do
        let livePeers' = Set.insert peer livePeers
        liftEffect $ log $ "Peer connected (live peers " <> show (Set.size livePeers') <> "/"
          <> show (Array.length config.peers)
          <> ")."
        pure livePeers'
      _, _ -> pure livePeers

msgPeerDisconnectedHandler :: forall m. AppBase m => PeerConnMessage -> m Unit
msgPeerDisconnectedHandler { peer } = do
  { config: AppConfig config, livePeers: livePeersAVar } <- accessRec
    (Proxy :: _ ("config" :> "livePeers" :> Nil'))
  liftAff $ modifyAVar_ livePeersAVar \livePeers ->
    case Set.member peer livePeers, peer /= config.hydraNodeId of
      true, true -> do
        let livePeers' = Set.delete peer livePeers
        liftEffect $ log $ "Peer disconnected (live peers " <> show (Set.size livePeers')
          <> "/"
          <> show (Array.length config.peers)
          <> ")."
        pure livePeers'
      _, _ -> pure livePeers

msgHeadIsInitializingHandler :: forall m. AppInit m => m Unit
msgHeadIsInitializingHandler = do
  setHeadStatus' HeadStatus_Initializing
  commitStatus <- readAppState (Proxy :: _ "commitStatus")
  when (commitStatus == ShouldCommitStandingBid) do
    runExceptT commitStandingBid >>=
      either
        ( \err ->
            liftEffect (log $ "Could not commit standing bid, error: " <> show err <> ".")
              *> setCommitStatus ShouldCommitCollateral
        )
        (const (pure unit))

msgCommittedHandler :: forall m. AppInit m => CommittedMessage -> m Unit
msgCommittedHandler _ =
  runExceptT commitCollateral >>=
    either
      (\err -> liftEffect $ log $ "Could not commit collateral, error: " <> show err <> ".")
      (const (pure unit))

msgHeadAbortedHandler :: forall m. AppInit m => m Unit
msgHeadAbortedHandler = do
  setHeadStatus' HeadStatus_Final
  throwError $ error $ "Head is finalized, stopping delegate-server."

msgHeadOpenHandler
  :: forall m
   . AppOpen m
  => DelegateWebSocketServer
  -> HeadOpenMessage
  -> m Unit
msgHeadOpenHandler wsServer { utxo } = do
  setHeadStatus' HeadStatus_Open
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
  => HydraNodeApiWebSocket
  -> HeadClosedMessage
  -> m Unit
msgHeadClosedHandler ws { snapshotNumber } = do
  setHeadStatus' HeadStatus_Closed
  ownSnapshot <- unwrap <$> readAppState (Proxy :: _ "snapshot")
  when (ownSnapshot.snapshotNumber > snapshotNumber) $
    liftEffect ws.challengeSnapshot

msgReadyToFanoutHandler :: forall m. AppBase m => HydraNodeApiWebSocket -> m Unit
msgReadyToFanoutHandler ws = do
  setHeadStatus' HeadStatus_FanoutPossible
  liftEffect $ ws.fanout

msgHeadFinalizedHandler :: forall m. AppBase m => HeadFinalizedMessage -> m Unit
msgHeadFinalizedHandler _ = do
  setHeadStatus' HeadStatus_Final
  exitWithReason AppExitReason_HeadFinalized

--

setHeadStatus' :: forall m. AppBase m => HydraHeadStatus -> m Unit
setHeadStatus' status = do
  setHeadStatus status
  liftEffect $ log $ "New head status: " <> printHeadStatus status <> "."

setSnapshot' :: forall m. AppOpen m => DelegateWebSocketServer -> HydraSnapshot -> m Unit
setSnapshot' wsServer snapshot = do
  setSnapshot snapshot
  liftEffect $ log $ "New confirmed snapshot: " <> printJsonUsingCodec hydraSnapshotCodec
    snapshot
  standingBid <- map snd <$> queryStandingBidL2
  liftEffect $ traverse_ wsServer.broadcast standingBid
