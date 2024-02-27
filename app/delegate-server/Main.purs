module DelegateServer.Main where

import Prelude

import Ansi.Output (dim, withGraphics)
import Data.Foldable (foldMap)
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.UInt (toString) as UInt
import DelegateServer.Config (AppConfig, configParser)
import DelegateServer.Const (appConst)
import DelegateServer.HydraNodeApi.WebSocket (mkHydraNodeApiWebSocket)
import DelegateServer.State (AppState, initApp, runAppEff)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HydraAuctionOffchain.Config (printHostPort)
import Node.ChildProcess (defaultSpawnOptions, spawn, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.Stream (onDataString)
import Options.Applicative ((<**>))
import Options.Applicative as Optparse

main :: Effect Unit
main = launchAff_ do
  appConfig <- liftEffect $ Optparse.execParser opts
  appState <- initApp appConfig
  liftEffect $ startHydraNode appState

opts :: Optparse.ParserInfo AppConfig
opts =
  Optparse.info (configParser <**> Optparse.helper) $ Optparse.fullDesc
    <> Optparse.header "delegate-server"

startHydraNode :: AppState -> Effect Unit
startHydraNode appState@{ config: appConfig } = do
  hydraNodeProcess <- spawn "hydra-node" hydraNodeArgs defaultSpawnOptions
  onDataString (stdout hydraNodeProcess) Encoding.UTF8 \str -> do
    log $ withGraphics dim $ "[hydra-node] " <> str
    when (String.contains (Pattern "APIServerStarted") str) do
      runAppEff appState $ mkHydraNodeApiWebSocket
  where
  peerArgs :: Array String
  peerArgs =
    appConfig.peers # foldMap \peer ->
      [ "--peer"
      , printHostPort peer.hydraNode
      , "--hydra-verification-key"
      , peer.hydraVk
      , "--cardano-verification-key"
      , peer.cardanoVk
      ]

  hydraNodeArgs :: Array String
  hydraNodeArgs =
    peerArgs <>
      [ "--node-id"
      , appConfig.hydraNodeId
      , "--host"
      , appConfig.hydraNode.host
      , "--port"
      , UInt.toString appConfig.hydraNode.port
      , "--api-host"
      , appConfig.hydraNodeApi.host
      , "--api-port"
      , UInt.toString appConfig.hydraNodeApi.port
      , "--persistence-dir"
      , appConfig.hydraPersistDir
      , "--hydra-signing-key"
      , appConfig.hydraSk
      , "--cardano-signing-key"
      , appConfig.cardanoSk
      , "--node-socket"
      , appConfig.nodeSocketPreprod
      , "--testnet-magic"
      , appConst.testnetMagic
      , "--ledger-protocol-parameters"
      , appConst.protocolParams
      , "--hydra-scripts-tx-id"
      , appConst.hydraScriptsTxHash
      ]
