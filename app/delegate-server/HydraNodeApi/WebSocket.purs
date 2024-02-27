module DelegateServer.HydraNodeApi.WebSocket
  ( mkHydraNodeApiWebSocket
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Array (length) as Array
import Data.Set (delete, insert, member, size) as Set
import Data.Tuple.Nested ((/\))
import DelegateServer.HydraNodeApi.Types.Message
  ( HydraNodeApi_InMsg(HydraNodeApi_InMsg_PeerConnected, HydraNodeApi_InMsg_PeerDisconnected)
  , HydraNodeApi_OutMsg(HydraNodeApi_OutMsg_Init)
  , hydraNodeApiInMsgCodec
  , hydraNodeApiOutMsgCodec
  )
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.State (AppM, runAppEff)
import DelegateServer.WebSocket (mkWebSocket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

type HydraNodeApiWebSocket =
  { initHead :: AppM Unit
  }

mkHydraNodeApiWebSocket :: AppM HydraNodeApiWebSocket
mkHydraNodeApiWebSocket = do
  appState <- ask
  ws /\ wsUrl <- mkWebSocket
    { hostPort: appState.config.hydraNodeApi
    , inMsgCodec: hydraNodeApiInMsgCodec
    , outMsgCodec: hydraNodeApiOutMsgCodec
    , runM: runAppEff appState
    }
  ws.onConnect $ connectHandler wsUrl
  ws.onMessage messageHandler
  pure
    { initHead: ws.send HydraNodeApi_OutMsg_Init
    }

----------------------------------------------------------------------
-- Handlers

connectHandler :: String -> AppM Unit
connectHandler wsUrl =
  liftEffect do
    log $ "Connected to hydra-node-api ws server (" <> wsUrl <> ")."

messageHandler :: HydraNodeApi_InMsg -> AppM Unit
messageHandler = case _ of
  HydraNodeApi_InMsg_PeerConnected { peer } ->
    msgPeerConnectedHandler peer
  HydraNodeApi_InMsg_PeerDisconnected { peer } ->
    msgPeerDisconnectedHandler peer

msgPeerConnectedHandler :: String -> AppM Unit
msgPeerConnectedHandler peer = do
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

msgPeerDisconnectedHandler :: String -> AppM Unit
msgPeerDisconnectedHandler peer = do
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
