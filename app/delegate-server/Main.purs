module DelegateServer.Main
  ( AppHandle
  , main
  , startDelegateServer
  ) where

import Prelude

import Ansi.Codes (Color(Red))
import Ansi.Output (foreground, withGraphics)
import Contract.Address (getNetworkId)
import Contract.Config (QueryBackendParams)
import Contract.Log (logInfo', logTrace', logWarn')
import Control.Parallel (parTraverse)
import Ctl.Internal.Helpers ((<</>>))
import Data.Foldable (foldMap)
import Data.Int (decimal, toStringAs) as Int
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Data.UInt (toString) as UInt
import DelegateServer.App
  ( AppLogger
  , AppM
  , appLoggerDefault
  , getAppEffRunner
  , initApp
  , runApp
  , runContract
  , runContractExitOnErr
  )
import DelegateServer.AppMap (AppMap, buildAppMap)
import DelegateServer.Cleanup (appCleanupHandler, appInstanceCleanupHandler)
import DelegateServer.Config
  ( AppConfig
  , AppConfig'(AppConfig)
  , Network(Testnet, Mainnet)
  , AuctionConfig
  , execAppConfigParser
  )
import DelegateServer.Const (appConst)
import DelegateServer.Contract.Collateral (getCollateralUtxo)
import DelegateServer.Contract.QueryAuction (queryAuction)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket, mkHydraNodeApiWebSocket)
import DelegateServer.Lib.Timer (scheduleAt)
import DelegateServer.Server (httpServer)
import DelegateServer.State
  ( class AppBase
  , class AppInit
  , access
  , exitWithReason
  , putAppState
  , readAppState
  , setAuctionInfo
  , setCollateralUtxo
  )
import DelegateServer.Types.AppExitReason
  ( AppExitReason(AppExitReason_BiddingTimeExpired_UnexpectedHeadStatus)
  )
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus
      ( HeadStatus_Unknown
      , HeadStatus_Idle
      , HeadStatus_Initializing
      , HeadStatus_Open
      )
  , isHeadClosed
  )
import DelegateServer.WsServer (DelegateWebSocketServer, wsServer)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (tryPut, tryTake) as AVarSync
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (empty, new, take, tryPut) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message)
import Effect.Timer (TimeoutId)
import HydraAuctionOffchain.Contract.Types (auctionInfoExtendedCodec)
import HydraAuctionOffchain.Helpers (waitSeconds)
import HydraAuctionOffchain.Lib.Json (printJsonUsingCodec)
import HydraAuctionOffchain.Types.HostPort (printHostPort)
import Node.ChildProcess (ChildProcess, defaultSpawnOptions, spawn, stderr, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.Process (onSignal, onUncaughtException)
import Node.Stream (onDataString)
import Type.Proxy (Proxy(Proxy))

main :: Effect Unit
main = launchAff_ do
  appConfig <- liftEffect execAppConfigParser
  appHandle <- startDelegateServer appConfig
  liftEffect do
    onUncaughtException \err -> do
      log $ withGraphics (foreground Red) $ message err
      appHandle.cleanupHandler
    onSignal SIGINT appHandle.cleanupHandler
    onSignal SIGTERM appHandle.cleanupHandler

type AppHandle =
  { appMap :: AppMap HydraNodeApiWebSocket
  , appLogger :: AppLogger
  , cleanupHandler :: Effect Unit
  }

startDelegateServer :: AppConfig' (Array AuctionConfig) QueryBackendParams -> Aff AppHandle
startDelegateServer appConfig@(AppConfig appConfigRec) = do
  -- TODO: Make each app instance have its own logger, otherwise the
  -- logs will be a complete mess
  let appLogger = appLoggerDefault

  appStateList <-
    parTraverse
      ( \auctionConfig -> do
          appState <- initApp appConfig auctionConfig
          runApp appState appLogger $ setAuction *> prepareCollateralUtxo
          pure appState
      )
      appConfigRec.auctionConfig -- array of auction configurations

  appMap0 <- buildAppMap $ flip Tuple unit <$> appStateList
  wsServer' <- wsServer appConfigRec.wsServerPort appConfigRec.network appMap0 appLogger

  appInstances <- parTraverse
    ( \appState ->
        runApp appState appLogger do
          hydraNodeProcess <- startHydraNode
          hydraNodeApiWs <- initHydraApiWsConn wsServer'
          timers <-
            sequence
              [ initHeadAtBiddingStart hydraNodeApiWs
              , closeHeadAtBiddingEnd hydraNodeApiWs
              ]

          contractEnv <- unwrap <$> access (Proxy :: _ "contractEnv")
          cleanupSem <- liftAff $ AVar.new unit
          let
            cleanupHandler exitReason =
              AVarSync.tryTake cleanupSem >>=
                maybe (pure unit) \_ -> do
                  log $ withGraphics (foreground Red) $ show exitReason
                  appInstanceCleanupHandler hydraNodeProcess hydraNodeApiWs contractEnv timers

          putAppState (Proxy :: _ "exit") cleanupHandler -- attach cleanup handler to each app instance
          pure { appMapValue: appState /\ hydraNodeApiWs, cleanupHandler }
    )
    appStateList

  appMap1 <- buildAppMap $ _.appMapValue <$> appInstances
  closeHttpServer <- liftEffect $ httpServer appConfigRec.serverPort appLogger appMap1

  cleanupSem <- liftAff $ AVar.new unit
  let
    cleanupHandler =
      AVarSync.tryTake cleanupSem >>=
        maybe (pure unit) \_ ->
          appCleanupHandler closeHttpServer wsServer' (_.cleanupHandler <$> appInstances)
  pure
    { appMap: appMap1
    , appLogger
    , cleanupHandler
    }

setAuction :: forall m. AppBase m => m Unit
setAuction = do
  { auctionConfig: { auctionMetadataOref } } <- unwrap <$> access (Proxy :: _ "config")
  auctionInfo <- runContractExitOnErr $ queryAuction auctionMetadataOref
  setAuctionInfo auctionInfo
  network <- runContract getNetworkId
  logInfo' $ "Got valid auction: " <> printJsonUsingCodec (auctionInfoExtendedCodec network)
    auctionInfo

prepareCollateralUtxo :: forall m. AppInit m => m Unit
prepareCollateralUtxo = do
  utxo <- runContract getCollateralUtxo
  setCollateralUtxo utxo
  logInfo' $ "Prepared collateral utxo: " <> show utxo

initHeadAtBiddingStart :: HydraNodeApiWebSocket -> AppM (AVar (Maybe TimeoutId))
initHeadAtBiddingStart ws = do
  auctionInfo <- readAppState (Proxy :: _ "auctionInfo")
  runAppEff' <- getAppEffRunner
  let
    biddingStart = (unwrap (unwrap auctionInfo).auctionTerms).biddingStart
    action =
      runAppEff' do
        headStatus <- readAppState (Proxy :: _ "headStatus")
        case headStatus of
          HeadStatus_Unknown -> do
            logInfo'
              "Bidding period active but head status is unknown - retrying in 5 seconds."
            waitSeconds 5
            liftEffect action
          HeadStatus_Idle -> do
            logInfo' "Bidding period active, initializing the head."
            liftEffect ws.initHead
          _ ->
            pure unit
  liftEffect $ scheduleAt biddingStart action

closeHeadAtBiddingEnd :: HydraNodeApiWebSocket -> AppM (AVar (Maybe TimeoutId))
closeHeadAtBiddingEnd ws = do
  auctionInfo <- readAppState (Proxy :: _ "auctionInfo")
  runAppEff' <- getAppEffRunner
  let
    biddingEnd = (unwrap (unwrap auctionInfo).auctionTerms).biddingEnd
    action =
      runAppEff' do
        headStatus <- readAppState (Proxy :: _ "headStatus")
        case headStatus of
          HeadStatus_Unknown -> do
            logInfo' "Bidding time expired but head status is unknown - retrying in 5 seconds."
            waitSeconds 5
            liftEffect action
          HeadStatus_Initializing -> do
            logInfo' "Bidding time expired, aborting the head."
            liftEffect ws.abortHead
          HeadStatus_Open -> do
            logInfo' "Bidding time expired, closing the head."
            liftEffect ws.closeHead
          _ | isHeadClosed headStatus ->
            -- No-op if the head is already closed.
            pure unit
          _ ->
            exitWithReason $
              AppExitReason_BiddingTimeExpired_UnexpectedHeadStatus
                headStatus
  liftEffect $ scheduleAt biddingEnd action

initHydraApiWsConn :: DelegateWebSocketServer -> AppM HydraNodeApiWebSocket
initHydraApiWsConn wsServer = do
  wsAVar <- liftAff AVar.empty
  mkHydraNodeApiWebSocket wsServer (liftAff <<< void <<< flip AVar.tryPut wsAVar)
  liftAff $ AVar.take wsAVar

startHydraNode :: AppM ChildProcess
startHydraNode = do
  appConfig <- access (Proxy :: _ "config")
  apiServerStartedSem <- liftAff AVar.empty
  hydraNodeProcess <- liftEffect $ spawn "hydra-node" (hydraNodeArgs appConfig)
    defaultSpawnOptions

  runAppEff' <- getAppEffRunner
  liftEffect $ onDataString (stderr hydraNodeProcess) Encoding.UTF8 \str ->
    runAppEff' $ logWarn' $ "[hydra-node:stderr] " <> str

  liftEffect $ onDataString (stdout hydraNodeProcess) Encoding.UTF8 \str -> do
    runAppEff' $ logTrace' $ "[hydra-node:stdout] " <> str
    when (String.contains (Pattern "APIServerStarted") str) do
      void $ AVarSync.tryPut unit apiServerStartedSem

  liftAff $ AVar.take apiServerStartedSem
  pure hydraNodeProcess
  where
  peerArgs :: AppConfig -> Array String
  peerArgs (AppConfig appConfig) =
    appConfig.auctionConfig.peers # foldMap \peer ->
      [ "--peer"
      , printHostPort peer.hydraNode
      , "--hydra-verification-key"
      , peer.hydraVk
      , "--cardano-verification-key"
      , peer.cardanoVk
      ]

  networkArgs :: AppConfig -> Array String
  networkArgs (AppConfig appConfig) =
    case appConfig.network of
      Testnet { magic } ->
        [ "--testnet-magic"
        , Int.toStringAs Int.decimal magic
        ]
      Mainnet ->
        [ "--mainnet"
        ]

  hydraNodeArgs :: AppConfig -> Array String
  hydraNodeArgs ac@(AppConfig appConfig) =
    peerArgs ac <> networkArgs ac <>
      [ "--node-id"
      , appConfig.auctionConfig.hydraNodeId
      , "--host"
      , appConfig.auctionConfig.hydraNode.host
      , "--port"
      , UInt.toString appConfig.auctionConfig.hydraNode.port
      , "--api-host"
      , appConfig.auctionConfig.hydraNodeApi.host
      , "--api-port"
      , UInt.toString appConfig.auctionConfig.hydraNodeApi.port
      , "--persistence-dir"
      , appConfig.hydraPersistDir <</>> appConfig.auctionConfig.hydraNodeId
      , "--hydra-signing-key"
      , appConfig.hydraSk
      , "--cardano-signing-key"
      , appConfig.auctionConfig.cardanoSk
      , "--node-socket"
      , appConfig.nodeSocket
      , "--ledger-protocol-parameters"
      , appConst.protocolParams
      , "--hydra-scripts-tx-id"
      , appConfig.hydraScriptsTxHash
      , "--contestation-period"
      , Int.toStringAs Int.decimal appConfig.hydraContestPeriod
      ]
