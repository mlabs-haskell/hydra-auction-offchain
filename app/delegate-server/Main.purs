module DelegateServer.Main
  ( AppHandle
  , main
  , startDelegateServer
  ) where

import Prelude

import Ansi.Codes (Color(Red))
import Ansi.Output (foreground, withGraphics)
import Contract.Config (QueryBackendParams)
import Data.Either (Either(Left, Right))
import Data.Map (values) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Traversable (traverse_)
import DelegateServer.AppManager (AppManager, initAppManager)
import DelegateServer.AppManager.Types (AuctionSlot)
import DelegateServer.Cleanup (appCleanupHandler)
import DelegateServer.Config (AppConfig'(AppConfig), AuctionSlotConfig, execAppConfigParser)
import DelegateServer.Server (httpServer)
import DelegateServer.WsServer (wsServer)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (take, tryRead, tryTake) as AVarSync
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (empty, new) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message)
import Effect.Exception (message) as Error
import Node.Process (onSignal, onUncaughtException)

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
  { appManager :: AVar AppManager
  , cleanupHandler :: Effect Unit
  }

startDelegateServer
  :: AppConfig' (Array (AuctionSlotConfig Maybe)) QueryBackendParams
  -> Aff AppHandle
startDelegateServer appConfig@(AppConfig appConfigRec) = do
  appManagerAvar <- AVar.empty
  wsServer' <- wsServer appConfigRec.wsServerPort appConfigRec.network appManagerAvar
  closeHttpServer <- liftEffect $ httpServer
    { serverPort: appConfigRec.serverPort
    , appManagerAvar
    , slotReservationPeriod: appConfigRec.slotReservationPeriod
    }
  initAppManager appConfig appManagerAvar wsServer'
  cleanupSem <- liftAff $ AVar.new unit
  let
    cleanupAppInstances exitReason =
      void $ AVarSync.take appManagerAvar case _ of
        Left err ->
          log $ "cleanupAppInstances: failed to take appManager AVar, error: "
            <> Error.message err
        Right appManager ->
          traverse_
            ( \{ appState, occupiedSlot } ->
                AVarSync.tryRead appState.exit >>= case _ of
                  Nothing ->
                    log $
                      "cleanupAppInstances: no cleanup handler attached to app instance at slot"
                        <> show (occupiedSlot :: AuctionSlot)
                  Just handler ->
                    handler exitReason
            )
            (Map.values (unwrap appManager).activeAuctions)
    cleanupHandler =
      AVarSync.tryTake cleanupSem >>=
        maybe (pure unit) \_ ->
          appCleanupHandler closeHttpServer wsServer' cleanupAppInstances
  pure
    { appManager: appManagerAvar
    , cleanupHandler
    }
