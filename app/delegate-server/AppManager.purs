module DelegateServer.AppManager
  ( AppManager'
  , HostAuctionError
      ( AuctionSlotNotAvailable
      , IncorrectReservationCode
      , MissingOrInvalidAuctionInfo
      )
  , initAppManager
  , hostAuction
  ) where

import Prelude

import Ansi.Codes (Color(Red))
import Ansi.Output (foreground, withGraphics)
import Cardano.AsCbor (encodeCbor)
import Cardano.Types (ScriptHash, TransactionInput)
import Contract.Address (getNetworkId)
import Contract.CborBytes (cborBytesToHex)
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
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(Identity))
import Data.Map (empty, fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import DelegateServer.App
  ( AppLogger
  , AppM
  , AppState
  , appLoggerDefault
  , getAppEffRunner
  , initApp
  , runApp
  , runContract
  )
import DelegateServer.Cleanup (appInstanceCleanupHandler)
import DelegateServer.Config (AppConfig, AppConfig'(AppConfig), DelegateServerConfig)
import DelegateServer.Const (appConst)
import DelegateServer.Contract.Collateral (getCollateralUtxo)
import DelegateServer.Contract.QueryAuction (QueryAuctionError, queryAuction)
import DelegateServer.Helpers (printOref)
import DelegateServer.HydraNodeApi.WebSocket (mkHydraNodeApiWebSocket)
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
import HydraSdk.Extra.AppManager
  ( AppManager
  , AppManagerSlot
  , ReservationCode
  , hostApp
  , removeApp
  , withAppManager
  )
import HydraSdk.Extra.AppManager (HostAppError(SlotNotAvailable, IncorrectReservationCode)) as HydraSdk
import HydraSdk.NodeApi (HydraNodeApiWebSocket)
import HydraSdk.Process (HydraHeadPeer) as HydraSdk
import HydraSdk.Process (HydraNodeStartupParams, spawnHydraNode)
import HydraSdk.Types
  ( HydraHeadStatus
      ( HeadStatus_Idle
      , HeadStatus_Initializing
      , HeadStatus_Open
      , HeadStatus_Unknown
      )
  , isHeadClosed
  )
import Node.ChildProcess (ChildProcess)
import Type.Proxy (Proxy(Proxy))

type AppManager' = AppManager
  ScriptHash
  { appState :: AppState
  , appLogger :: AppLogger
  , hydraNodeApiWs :: HydraNodeApiWebSocket AppM
  }
  (AppConfig Maybe)
  (AppConfig Identity)

initAppManager
  :: DelegateServerConfig
  -> AVar AppManager'
  -> DelegateWebSocketServer
  -> Aff Unit
initAppManager (AppConfig appConfig) appManagerAvar wsServer = do
  let
    appManager =
      { activeApps: Map.empty
      , reservedSlots: Map.empty
      , availableSlots:
          Map.fromFoldable $ Array.zip (zero .. Array.length slotConfigs)
            slotConfigs
      }
  appManager' <-
    foldM
      ( \appManagerAcc (slot /\ AppConfig { auctionConfig: { auctionMetadataOref } }) ->
          case auctionMetadataOref of
            Nothing -> pure appManagerAcc
            Just oref -> do
              hostAuctionRes <- hostAuction
                { slot
                , reservationCode: Nothing
                , appManager: appManagerAcc
                , appManagerAvar
                , auctionMetadataOref: oref
                , wsServer
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
      (Map.toUnfoldable appManager.availableSlots)
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
  { slot :: AppManagerSlot
  , reservationCode :: Maybe ReservationCode
  , appManager :: AppManager'
  , appManagerAvar :: AVar AppManager'
  , auctionMetadataOref :: TransactionInput
  , wsServer :: DelegateWebSocketServer
  }

data HostAuctionError
  = AuctionSlotNotAvailable
  | IncorrectReservationCode
  | MissingOrInvalidAuctionInfo QueryAuctionError

derive instance Generic HostAuctionError _

instance Show HostAuctionError where
  show = genericShow

toHostAuctionError :: HydraSdk.HostAppError -> HostAuctionError
toHostAuctionError = case _ of
  HydraSdk.SlotNotAvailable -> AuctionSlotNotAvailable
  HydraSdk.IncorrectReservationCode -> IncorrectReservationCode

hostAuction :: HostAuctionParams -> Aff (Either HostAuctionError AppManager')
hostAuction params =
  map (join <<< map (lmap toHostAuctionError)) $ runExceptT $ hostApp
    { slot: params.slot
    , reservationCode: params.reservationCode
    , appManager: params.appManager
    , startApp: \(AppConfig appConfigAvailable) -> do
        let
          appConfig =
            wrap $ appConfigAvailable
              { auctionConfig = appConfigAvailable.auctionConfig
                  { auctionMetadataOref = Identity params.auctionMetadataOref
                  }
              }
        appState <- lift $ initApp appConfig
        let appLogger = appLoggerDefault
        auctionId <- ExceptT $ lmap MissingOrInvalidAuctionInfo <$>
          runApp appState appLogger setAuction
        hydraNodeApiWs <- lift $ runApp appState appLogger do
          prepareCollateralUtxo
          hydraNodeProcess <- startHydraNode
          hydraNodeApiWs <- initHydraApiWsConn params.wsServer
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
                  launchAff_ $ withAppManager params.appManagerAvar \appManager -> do
                    mAppManager' <- removeApp auctionId appManager
                      { deactivateConfig: \(AppConfig activeConfig) ->
                          pure $ wrap $ activeConfig
                            { auctionConfig = activeConfig.auctionConfig
                                { auctionMetadataOref = Nothing
                                }
                            }
                      }
                    maybe
                      ( do
                          liftEffect $ log $
                            "Could not find active auction in AppManager, auction id: "
                              <> show auctionId
                          pure $ appManager /\ unit
                      )
                      (pure <<< flip Tuple unit)
                      mAppManager'
          putAppState (Proxy :: _ "exit") cleanupHandler -- attach cleanup handler to each app instance
          pure hydraNodeApiWs
        liftEffect $ log $ "Hosting auction with id: " <> cborBytesToHex (encodeCbor auctionId)
        pure
          { id: auctionId
          , state: { appState, appLogger, hydraNodeApiWs }
          , config: appConfig
          }
    }

initHeadAtBiddingStart :: forall m. HydraNodeApiWebSocket m -> AppM (AVar (Maybe TimeoutId))
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

closeHeadAtBiddingEnd :: forall m. HydraNodeApiWebSocket m -> AppM (AVar (Maybe TimeoutId))
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

initHydraApiWsConn :: DelegateWebSocketServer -> AppM (HydraNodeApiWebSocket AppM)
initHydraApiWsConn wsServer = do
  wsAVar <- liftAff AVar.empty
  mkHydraNodeApiWebSocket wsServer (liftAff <<< void <<< flip AVar.tryPut wsAVar)
  liftAff $ AVar.take wsAVar

startHydraNode :: AppM ChildProcess
startHydraNode = do
  AppConfig appConfig@{ auctionConfig } <- access (Proxy :: _ "config")
  let
    peers :: Array HydraSdk.HydraHeadPeer
    peers =
      auctionConfig.peers <#> \peer ->
        { hydraNodeAddress: peer.hydraNode
        , hydraVerificationKey: peer.hydraVk
        , cardanoVerificationKey: peer.cardanoVk
        }

    startupParams :: HydraNodeStartupParams
    startupParams =
      { nodeId: auctionConfig.hydraNodeId
      , hydraNodeAddress: auctionConfig.hydraNode
      , hydraNodeApiAddress: auctionConfig.hydraNodeApi
      , persistDir: appConfig.hydraPersistDir <</>> auctionConfig.hydraNodeId
      , hydraSigningKey: appConfig.hydraSk
      , cardanoSigningKey: auctionConfig.cardanoSk
      , network: appConfig.network
      , nodeSocket: appConfig.nodeSocket
      , pparams: appConst.protocolParams
      , hydraScriptsTxHash: appConfig.hydraScriptsTxHash
      , contestPeriodSec: appConfig.hydraContestPeriod
      , peers
      }
  apiServerStartedSem <- liftAff AVar.empty
  runAppEff' <- getAppEffRunner
  hydraNodeProcess <- spawnHydraNode startupParams
    { apiServerStartedHandler:
        Just $ void $ AVarSync.tryPut unit apiServerStartedSem
    , stdoutHandler:
        Just $ \str -> runAppEff' $ logTrace' $ "[hydra-node:stdout] " <> str
    , stderrHandler:
        Just $ \str -> runAppEff' $ logWarn' $ "[hydra-node:stderr] " <> str
    }
  liftAff $ AVar.take apiServerStartedSem
  pure hydraNodeProcess
