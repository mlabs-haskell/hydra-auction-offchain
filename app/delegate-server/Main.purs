module DelegateServer.Main where

import Prelude

import Ansi.Output (dim, withGraphics)
import Contract.Monad (ContractEnv, stopContractEnv)
import Control.Monad.Reader (asks)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Posix.Signal (toString) as Signal
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.UInt (toString) as UInt
import DelegateServer.ClientServer.Server (clientServer)
import DelegateServer.Config (AppConfig, configParser)
import DelegateServer.Const (appConst)
import DelegateServer.Contract.Collateral (getCollateralUtxo)
import DelegateServer.Contract.QueryAuction (queryAuction)
import DelegateServer.HydraNodeApi.WebSocket (mkHydraNodeApiWebSocket)
import DelegateServer.Lib.Json (printJsonUsingCodec)
import DelegateServer.State
  ( AppM
  , AppState
  , initApp
  , runApp
  , runAppEff
  , runContract
  , runContractExitOnErr
  , setAuctionInfo
  , setCollateralUtxo
  )
import Effect (Effect)
import Effect.AVar (new, tryTake) as AVar
import Effect.Aff (launchAff_, runAff_)
import Effect.Aff.AVar (AVar)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HydraAuctionOffchain.Config (printHostPort)
import HydraAuctionOffchain.Contract.Types (auctionInfoExtendedCodec)
import Node.ChildProcess (defaultSpawnOptions, spawn, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.Process (onSignal)
import Node.Stream (onDataString)
import Options.Applicative ((<**>))
import Options.Applicative as Optparse

main :: Effect Unit
main = launchAff_ do
  appConfig <- liftEffect $ Optparse.execParser opts
  appState <- initApp appConfig
  runApp appState $ setAuction *> prepareCollateralUtxo
  liftEffect $ startServices appState

opts :: Optparse.ParserInfo AppConfig
opts =
  Optparse.info (configParser <**> Optparse.helper) $ Optparse.fullDesc
    <> Optparse.header "delegate-server"

setAuction :: AppM Unit
setAuction = do
  { auctionMetadataOref } <- asks _.config
  auctionInfo <- runContractExitOnErr $ queryAuction auctionMetadataOref
  setAuctionInfo auctionInfo
  liftEffect $ log $ "Got valid auction: " <> printJsonUsingCodec auctionInfoExtendedCodec
    auctionInfo

prepareCollateralUtxo :: AppM Unit
prepareCollateralUtxo = do
  utxo <- runContract getCollateralUtxo
  setCollateralUtxo utxo
  liftEffect $ log $ "Prepared collateral utxo: " <> show utxo

startServices :: AppState -> Effect Unit
startServices appState@{ config: appConfig } = do
  hydraNodeProcess <- spawn "hydra-node" hydraNodeArgs defaultSpawnOptions
  onDataString (stdout hydraNodeProcess) Encoding.UTF8 \str -> do
    log $ withGraphics dim $ "[hydra-node] " <> str
    when (String.contains (Pattern "APIServerStarted") str) do
      runAppEff appState do
        mkHydraNodeApiWebSocket \ws ->
          liftEffect do
            sem <- AVar.new unit
            closeClientServer <- clientServer appState ws
            let
              closeWs = ws.baseWs.close
              addCleanupHandler' sig =
                addCleanupHandler sig closeClientServer closeWs appState.contractEnv sem
            addCleanupHandler' SIGINT *> addCleanupHandler' SIGTERM
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

addCleanupHandler
  :: Signal
  -> (Effect Unit -> Effect Unit)
  -> Effect Unit
  -> ContractEnv
  -> AVar Unit
  -> Effect Unit
addCleanupHandler sig closeClientServer closeWs contractEnv sem =
  onSignal sig $
    AVar.tryTake sem >>= case _ of
      Nothing ->
        log $ "\nReceived " <> Signal.toString sig <>
          ", not executing cleanup handlers twice, doing nothing."
      Just _ -> do
        log $ "\nReceived " <> Signal.toString sig <> ", executing cleanup handlers."
        log "Stopping client server."
        closeClientServer $ pure unit
        log "Closing hydra-node-api ws connection."
        closeWs
        log "Finalizing Contract environment."
        flip runAff_ (stopContractEnv contractEnv) $
          const (log "Successfully completed all cleanup actions -> exiting.")
