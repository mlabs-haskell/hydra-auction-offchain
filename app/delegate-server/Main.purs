module DelegateServer.Main
  ( AppHandle
  , main
  , startDelegateServer
  ) where

import Prelude

import Ansi.Codes (Color(Red))
import Ansi.Output (foreground, withGraphics)
import Contract.Monad (ContractEnv, stopContractEnv)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Foldable (foldMap)
import Data.Int (decimal, toStringAs) as Int
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.Traversable (for_, sequence, traverse_)
import Data.UInt (toString) as UInt
import DelegateServer.App
  ( AppM
  , AppState
  , initApp
  , runApp
  , runAppEff
  , runContract
  , runContractExitOnErr
  )
import DelegateServer.Config (AppConfig(AppConfig), Network(Testnet, Mainnet), configParser)
import DelegateServer.Const (appConst)
import DelegateServer.Contract.Collateral (getCollateralUtxo)
import DelegateServer.Contract.QueryAuction (queryAuction)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket, mkHydraNodeApiWebSocket)
import DelegateServer.Lib.Timer (scheduleAt)
import DelegateServer.Server (server)
import DelegateServer.State
  ( class AppBase
  , class AppInit
  , access
  , readAppState
  , setAuctionInfo
  , setCollateralUtxo
  )
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus
      ( HeadStatus_Unknown
      , HeadStatus_Idle
      , HeadStatus_Initializing
      , HeadStatus_Open
      )
  , isHeadClosed
  , printHeadStatus
  )
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (take, tryPut, tryTake) as AVarSync
import Effect.Aff (Aff, launchAff_, runAff_)
import Effect.Aff.AVar (empty, new, take, tryPut) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, message)
import Effect.Timer (TimeoutId)
import Effect.Timer (clearTimeout) as Timer
import HydraAuctionOffchain.Config (printHostPort)
import HydraAuctionOffchain.Contract.Types (auctionInfoExtendedCodec)
import HydraAuctionOffchain.Helpers (waitSeconds)
import HydraAuctionOffchain.Lib.Json (printJsonUsingCodec)
import Node.ChildProcess (ChildProcess, defaultSpawnOptions, kill, spawn, stderr, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.Process (onSignal, onUncaughtException)
import Node.Stream (onDataString)
import Options.Applicative ((<**>))
import Options.Applicative as Optparse
import Type.Proxy (Proxy(Proxy))

main :: Effect Unit
main = launchAff_ do
  appConfig <- liftEffect $ Optparse.execParser opts
  appHandle <- startDelegateServer appConfig
  liftEffect do
    onUncaughtException \err -> do
      log $ withGraphics (foreground Red) $ message err
      appHandle.cleanupHandler
    onSignal SIGINT appHandle.cleanupHandler
    onSignal SIGTERM appHandle.cleanupHandler
  exitReason <- AVar.take appHandle.appState.exitSem
  liftEffect do
    log $ withGraphics (foreground Red) $ show exitReason
    appHandle.cleanupHandler

opts :: Optparse.ParserInfo AppConfig
opts =
  Optparse.info (configParser <**> Optparse.helper) $ Optparse.fullDesc
    <> Optparse.header "delegate-server"

type AppHandle =
  { cleanupHandler :: Effect Unit
  , appState :: AppState
  , ws :: HydraNodeApiWebSocket
  }

startDelegateServer :: AppConfig -> Aff AppHandle
startDelegateServer appConfig = do
  appState <- initApp appConfig
  runApp appState $ setAuction *> prepareCollateralUtxo

  hydraNodeProcess <- startHydraNode appConfig
  ws <- initHydraApiWsConn appState
  closeServer <- liftEffect $ server appState ws

  timers <- runApp appState $
    sequence
      [ initHeadAtBiddingStart ws, closeHeadAtBiddingEnd ws ]

  cleanupSem <- AVar.new unit
  let
    cleanupHandler' =
      AVarSync.tryTake cleanupSem >>=
        maybe (pure unit)
          ( const
              ( cleanupHandler hydraNodeProcess ws closeServer (unwrap appState.contractEnv)
                  timers
              )
          )
  pure
    { cleanupHandler: cleanupHandler'
    , appState
    , ws
    }

cleanupHandler
  :: ChildProcess
  -> HydraNodeApiWebSocket
  -> (Effect Unit -> Effect Unit)
  -> ContractEnv
  -> Array (AVar (Maybe TimeoutId))
  -> Effect Unit
cleanupHandler hydraNodeProcess ws closeServer contractEnv timers = do
  log "\nCancelling timers."
  for_ timers \timer ->
    AVarSync.take timer \timeout -> do
      traverse_ (traverse_ Timer.clearTimeout) timeout
  log "Stopping http server."
  closeServer do
    log "Closing hydra-node-api ws connection."
    ws.baseWs.close
    log "Killing hydra-node."
    kill SIGINT hydraNodeProcess
    log "Finalizing Contract environment."
    flip runAff_ (stopContractEnv contractEnv) $
      const (log "Successfully completed all cleanup actions -> exiting.")

setAuction :: forall m. AppBase m => m Unit
setAuction = do
  { auctionMetadataOref } <- unwrap <$> access (Proxy :: _ "config")
  auctionInfo <- runContractExitOnErr $ queryAuction auctionMetadataOref
  setAuctionInfo auctionInfo
  liftEffect $ log $ "Got valid auction: " <> printJsonUsingCodec auctionInfoExtendedCodec
    auctionInfo

prepareCollateralUtxo :: forall m. AppInit m => m Unit
prepareCollateralUtxo = do
  utxo <- runContract getCollateralUtxo
  setCollateralUtxo utxo
  liftEffect $ log $ "Prepared collateral utxo: " <> show utxo

initHeadAtBiddingStart :: HydraNodeApiWebSocket -> AppM (AVar (Maybe TimeoutId))
initHeadAtBiddingStart ws = do
  appState <- ask
  auctionInfo <- readAppState (Proxy :: _ "auctionInfo")
  let
    biddingStart = (unwrap (unwrap auctionInfo).auctionTerms).biddingStart
    action =
      runAppEff appState do
        headStatus <- readAppState (Proxy :: _ "headStatus")
        case headStatus of
          HeadStatus_Unknown -> do
            liftEffect $ log
              "Bidding period active but head status is unknown - retrying in 5 seconds."
            waitSeconds 5
            liftEffect action
          HeadStatus_Idle ->
            liftEffect do
              log "Bidding period active, initializing the head."
              ws.initHead
          _ ->
            pure unit
  liftEffect $ scheduleAt biddingStart action

closeHeadAtBiddingEnd :: HydraNodeApiWebSocket -> AppM (AVar (Maybe TimeoutId))
closeHeadAtBiddingEnd ws = do
  appState <- ask
  auctionInfo <- readAppState (Proxy :: _ "auctionInfo")
  let
    biddingEnd = (unwrap (unwrap auctionInfo).auctionTerms).biddingEnd
    action =
      runAppEff appState do
        headStatus <- readAppState (Proxy :: _ "headStatus")
        case headStatus of
          HeadStatus_Unknown -> do
            liftEffect $ log
              "Bidding time expired but head status is unknown - retrying in 5 seconds."
            waitSeconds 5
            liftEffect action
          HeadStatus_Initializing ->
            liftEffect do
              log "Bidding time expired, aborting the head."
              ws.abortHead
          HeadStatus_Open ->
            liftEffect do
              log "Bidding time expired, closing the head."
              ws.closeHead
          _ | isHeadClosed headStatus ->
            -- No-op if the head is already closed.
            pure unit
          _ ->
            -- Terminate with an error if the head is neither open nor closed.
            throwError $ error $ "Bidding time expired, unexpected head status: "
              <> printHeadStatus headStatus
              <> "."
  liftEffect $ scheduleAt biddingEnd action

initHydraApiWsConn :: AppState -> Aff HydraNodeApiWebSocket
initHydraApiWsConn appState = do
  wsAVar <- AVar.empty
  runApp appState $ mkHydraNodeApiWebSocket \ws ->
    liftAff $ void $ AVar.tryPut ws wsAVar
  AVar.take wsAVar

startHydraNode :: AppConfig -> Aff ChildProcess
startHydraNode (AppConfig appConfig) = do
  apiServerStartedSem <- AVar.empty
  hydraNodeProcess <- liftEffect $ spawn "hydra-node" hydraNodeArgs defaultSpawnOptions

  liftEffect $ onDataString (stderr hydraNodeProcess) Encoding.UTF8 \str ->
    log $ withGraphics (foreground Red) $ "[hydra-node:stderr] " <> str

  liftEffect $ onDataString (stdout hydraNodeProcess) Encoding.UTF8 \str -> do
    -- log $ withGraphics dim $ "[hydra-node:stdout] " <> str
    when (String.contains (Pattern "APIServerStarted") str) do
      void $ AVarSync.tryPut unit apiServerStartedSem

  AVar.take apiServerStartedSem
  pure hydraNodeProcess
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

  networkArgs :: Array String
  networkArgs =
    case appConfig.network of
      Testnet magic ->
        [ "--testnet-magic"
        , Int.toStringAs Int.decimal magic
        ]
      Mainnet ->
        [ "--mainnet"
        ]

  hydraNodeArgs :: Array String
  hydraNodeArgs =
    peerArgs <> networkArgs <>
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
      , appConfig.nodeSocket
      , "--ledger-protocol-parameters"
      , appConst.protocolParams
      , "--hydra-scripts-tx-id"
      , appConfig.hydraScriptsTxHash
      , "--contestation-period"
      , Int.toStringAs Int.decimal appConfig.hydraContestPeriod
      ]
