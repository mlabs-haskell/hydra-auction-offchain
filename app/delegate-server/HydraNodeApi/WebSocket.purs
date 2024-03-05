module DelegateServer.HydraNodeApi.WebSocket
  ( HydraNodeApiWebSocket
  , mkHydraNodeApiWebSocket
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Array (length) as Array
import Data.Set (delete, insert, member, size) as Set
import Data.Tuple.Nested ((/\))
import DelegateServer.Contract.Commit (commitCollateral, commitStandingBid)
import DelegateServer.HydraNodeApi.Types.Message
  ( GreetingsMessage
  , HydraNodeApi_InMessage
      ( In_Greetings
      , In_PeerConnected
      , In_PeerDisconnected
      , In_HeadIsInitializing
      , In_Committed
      )
  , HydraNodeApi_OutMessage(Out_Init)
  , PeerConnMessage
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  )
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.State (AppM, runAppEff, setHeadStatus, whenCommitLeader)
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus(HeadStatus_Initializing)
  , printHeadStatus
  )
import DelegateServer.WebSocket (WebSocket, mkWebSocket)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

type HydraNodeApiWebSocket =
  { baseWs :: WebSocket AppM HydraNodeApi_InMessage HydraNodeApi_OutMessage
  , initHead :: Effect Unit
  }

mkHydraNodeApiWebSocket :: (HydraNodeApiWebSocket -> AppM Unit) -> AppM Unit
mkHydraNodeApiWebSocket onConnect = do
  appState <- ask
  ws /\ wsUrl <- mkWebSocket
    { hostPort: appState.config.hydraNodeApi
    , inMsgCodec: hydraNodeApiInMessageCodec
    , outMsgCodec: hydraNodeApiOutMessageCodec
    , runM: runAppEff appState
    }
  let
    hydraNodeApiWs :: HydraNodeApiWebSocket
    hydraNodeApiWs =
      { initHead: ws.send Out_Init
      , baseWs: ws
      }
  ws.onConnect $ connectHandler wsUrl *> onConnect hydraNodeApiWs
  ws.onMessage (messageHandler hydraNodeApiWs)
  ws.onError errorHandler

----------------------------------------------------------------------
-- Handlers

connectHandler :: String -> AppM Unit
connectHandler wsUrl =
  liftEffect do
    log $ "Connected to hydra-node-api ws server (" <> wsUrl <> ")."

errorHandler :: String -> AppM Unit
errorHandler err =
  liftEffect do
    log $ "hydra-node-api ws error: " <> err

messageHandler :: HydraNodeApiWebSocket -> HydraNodeApi_InMessage -> AppM Unit
messageHandler _ws = case _ of
  In_Greetings msg ->
    msgGreetingsHandler msg
  In_PeerConnected msg ->
    msgPeerConnectedHandler msg
  In_PeerDisconnected msg ->
    msgPeerDisconnectedHandler msg
  In_HeadIsInitializing ->
    msgHeadIsInitializingHandler
  In_Committed ->
    msgCommittedHandler

msgGreetingsHandler :: GreetingsMessage -> AppM Unit
msgGreetingsHandler { headStatus } = do
  setHeadStatus headStatus
  liftEffect $ log $ "New head status: " <> printHeadStatus headStatus <> "."

msgPeerConnectedHandler :: PeerConnMessage -> AppM Unit
msgPeerConnectedHandler { peer } = do
  appState <- ask
  liftAff $ modifyAVar_ appState.livePeersAVar \livePeers ->
    case Set.member peer livePeers, peer /= appState.config.hydraNodeId of
      false, true -> do
        let livePeers' = Set.insert peer livePeers
        liftEffect $ log $ "Peer connected (live peers " <> show (Set.size livePeers') <> "/"
          <> show (Array.length appState.config.peers)
          <> ")."
        pure livePeers'
      _, _ -> pure livePeers

msgPeerDisconnectedHandler :: PeerConnMessage -> AppM Unit
msgPeerDisconnectedHandler { peer } = do
  appState <- ask
  liftAff $ modifyAVar_ appState.livePeersAVar \livePeers ->
    case Set.member peer livePeers, peer /= appState.config.hydraNodeId of
      true, true -> do
        let livePeers' = Set.delete peer livePeers
        liftEffect $ log $ "Peer disconnected (live peers " <> show (Set.size livePeers')
          <> "/"
          <> show (Array.length appState.config.peers)
          <> ")."
        pure livePeers'
      _, _ -> pure livePeers

msgHeadIsInitializingHandler :: AppM Unit
msgHeadIsInitializingHandler = do
  setHeadStatus HeadStatus_Initializing
  whenCommitLeader commitStandingBid

msgCommittedHandler :: AppM Unit
msgCommittedHandler = commitCollateral
