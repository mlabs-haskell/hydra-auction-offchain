module DelegateServer.AppManager
  ( AppManager
  , initAppManager
  ) where

import Prelude

import Ansi.Codes (Color(Red))
import Ansi.Output (foreground, withGraphics)
import Cardano.Types (ScriptHash, TransactionInput)
import Contract.Address (getNetworkId)
import Contract.Config (QueryBackendParams)
import Contract.Log (logInfo', logTrace', logWarn')
import Control.Error.Util (bool)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Safely (foldM)
import Ctl.Internal.Helpers ((<</>>))
import Data.Array ((..))
import Data.Array (length, zip) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(Identity))
import Data.Int (decimal, toStringAs) as Int
import Data.Map (delete, empty, fromFoldable, insert, lookup, pop, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.UInt (toString) as UInt
import DelegateServer.App
  ( AppM
  , appLoggerDefault
  , getAppEffRunner
  , initApp
  , runApp
  , runContract
  )
import DelegateServer.AppManager.Types (AppManager'(AppManager), AuctionSlot)
import DelegateServer.Cleanup (appInstanceCleanupHandler)
import DelegateServer.Config
  ( AppConfig
  , AppConfig'(AppConfig)
  , Network(Testnet, Mainnet)
  , AuctionSlotConfig
  )
import DelegateServer.Const (appConst)
import DelegateServer.Contract.Collateral (getCollateralUtxo)
import DelegateServer.Contract.QueryAuction (QueryAuctionError, queryAuction)
import DelegateServer.Helpers (printOref)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket, mkHydraNodeApiWebSocket)
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.Lib.Timer (scheduleAt)
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
import DelegateServer.WsServer (DelegateWebSocketServer)
import Effect.AVar (tryPut, tryTake) as AVarSync
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (empty, new, take, tryPut) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Timer (TimeoutId)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended(AuctionInfoExtended)
  , auctionInfoExtendedCodec
  )
import HydraAuctionOffchain.Helpers (waitSeconds)
import HydraAuctionOffchain.Lib.Json (printJsonUsingCodec)
import HydraAuctionOffchain.Types.HostPort (printHostPort)
import Node.ChildProcess (ChildProcess, defaultSpawnOptions, spawn, stderr, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.Stream (onDataString)
import Type.Proxy (Proxy(Proxy))

type AppManager = AppManager' HydraNodeApiWebSocket DelegateWebSocketServer

initAppManager
  :: AppConfig' (Array (AuctionSlotConfig Maybe)) QueryBackendParams
  -> AVar AppManager
  -> DelegateWebSocketServer
  -> Aff Unit
initAppManager (AppConfig appConfig) appManagerAvar wsServer = do
  let
    appManager = AppManager
      { activeAuctions: Map.empty
      , availableSlots:
          Map.fromFoldable $ Array.zip (zero .. Array.length slotConfigs)
            slotConfigs
      , wsServer
      }
  appManager' <-
    foldM
      ( \appManagerAcc (slot /\ AppConfig { auctionConfig: { auctionMetadataOref } }) ->
          case auctionMetadataOref of
            Nothing -> pure appManagerAcc
            Just oref -> do
              hostAuctionRes <- hostAuction
                { slot
                , auctionMetadataOref: oref
                , appManager: appManagerAcc
                , appManagerAvar
                }
              either
                ( \appManagerErr -> do
                    liftEffect $ log $ "Could not host auction with metadata output reference "
                      <> printOref oref
                      <> " , reason: "
                      <> show appManagerErr
                    pure appManagerAcc
                )
                pure
                hostAuctionRes
      )
      appManager
      (Map.toUnfoldable (unwrap appManager).availableSlots)
  AVar.tryPut appManager' appManagerAvar >>=
    bool
      (throwError $ error "initAppManager: appManager AVar is not empty")
      (pure unit)
  where
  slotConfigs :: Array (AppConfig Maybe)
  slotConfigs =
    AppConfig <<< appConfig { auctionConfig = _ } <$>
      appConfig.auctionConfig

type HostAuctionParams =
  { slot :: AuctionSlot
  , auctionMetadataOref :: TransactionInput
  , appManager :: AppManager
  , appManagerAvar :: AVar AppManager
  }

data HostAuctionError
  = AuctionSlotNotAvailable
  | MissingOrInvalidAuctionInfo QueryAuctionError

derive instance Generic HostAuctionError _

instance Show HostAuctionError where
  show = genericShow

hostAuction :: HostAuctionParams -> Aff (Either HostAuctionError AppManager)
hostAuction { slot, auctionMetadataOref, appManager: AppManager appManager, appManagerAvar } =
  runExceptT do
    case Map.lookup slot appManager.availableSlots of
      Nothing ->
        throwError AuctionSlotNotAvailable
      Just (AppConfig appConfigAvailable) -> do
        let
          appConfig =
            wrap $ appConfigAvailable
              { auctionConfig = appConfigAvailable.auctionConfig
                  { auctionMetadataOref = Identity auctionMetadataOref
                  }
              }
        appState <- lift $ initApp appConfig
        let appLogger = appLoggerDefault
        auctionId <- ExceptT $ lmap MissingOrInvalidAuctionInfo <$>
          runApp appState appLogger setAuction
        hydraNodeApiWs <- lift $ runApp appState appLogger do
          prepareCollateralUtxo
          hydraNodeProcess <- startHydraNode
          hydraNodeApiWs <- initHydraApiWsConn appManager.wsServer
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
                  launchAff_ $ modifyAVar_ appManagerAvar \appManager' ->
                    maybe
                      ( do
                          liftEffect $ log $
                            "Could not find active auction in AppManager, auction id: "
                              <> show auctionId
                          pure appManager'
                      )
                      pure
                      (removeAuction auctionId appManager')
          putAppState (Proxy :: _ "exit") cleanupHandler -- attach cleanup handler to each app instance
          pure hydraNodeApiWs
        pure $ AppManager $ appManager
          { availableSlots = Map.delete slot appManager.availableSlots
          , activeAuctions =
              Map.insert auctionId
                { appState
                , appLogger
                , hydraNodeApiWs
                , occupiedSlot: slot
                }
                appManager.activeAuctions
          }

removeAuction :: ScriptHash -> AppManager -> Maybe AppManager
removeAuction auctionId (AppManager appManager) = do
  { appState, occupiedSlot } /\ activeAuctions <- Map.pop auctionId appManager.activeAuctions
  let
    appConfig = unwrap appState.config
    appConfigAvailable =
      wrap $ appConfig
        { auctionConfig = appConfig.auctionConfig
            { auctionMetadataOref = Nothing
            }
        }
  pure $ AppManager $ appManager
    { activeAuctions = activeAuctions
    , availableSlots =
        Map.insert occupiedSlot appConfigAvailable
          appManager.availableSlots
    }

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

setAuction :: forall m. AppBase m => m (Either QueryAuctionError ScriptHash)
setAuction = do
  { auctionConfig: { auctionMetadataOref } } <- unwrap <$> access (Proxy :: _ "config")
  runContract (queryAuction $ unwrap auctionMetadataOref) >>=
    case _ of
      Left queryAuctionErr ->
        pure $ Left queryAuctionErr
      Right auctionInfo@(AuctionInfoExtended { auctionId }) -> do
        setAuctionInfo auctionInfo
        network <- runContract getNetworkId
        logInfo' $ "Got valid auction: " <> printJsonUsingCodec
          (auctionInfoExtendedCodec network)
          auctionInfo
        pure $ Right auctionId

prepareCollateralUtxo :: forall m. AppInit m => m Unit
prepareCollateralUtxo = do
  utxo <- runContract getCollateralUtxo
  setCollateralUtxo utxo
  logInfo' $ "Prepared collateral utxo: " <> show utxo

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
  peerArgs :: AppConfig Identity -> Array String
  peerArgs (AppConfig appConfig) =
    appConfig.auctionConfig.peers # foldMap \peer ->
      [ "--peer"
      , printHostPort peer.hydraNode
      , "--hydra-verification-key"
      , peer.hydraVk
      , "--cardano-verification-key"
      , peer.cardanoVk
      ]

  networkArgs :: AppConfig Identity -> Array String
  networkArgs (AppConfig appConfig) =
    case appConfig.network of
      Testnet { magic } ->
        [ "--testnet-magic"
        , Int.toStringAs Int.decimal magic
        ]
      Mainnet ->
        [ "--mainnet"
        ]

  hydraNodeArgs :: AppConfig Identity -> Array String
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
