module DelegateServer.Cleanup
  ( appCleanupHandler
  , appInstanceCleanupHandler
  , cancelTimers
  ) where

import Prelude

import Contract.Monad (ContractEnv, stopContractEnv)
import Data.Maybe (Maybe)
import Data.Posix.Signal (Signal(SIGTERM))
import Data.Traversable (traverse_)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.Types.AppExitReason (AppExitReason(AppExitReason_Cleanup))
import DelegateServer.WsServer (DelegateWebSocketServer)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (take) as AVarSync
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Effect.Timer (TimeoutId)
import Effect.Timer (clearTimeout) as Timer
import Node.ChildProcess (ChildProcess, kill)

appCleanupHandler
  :: (Effect Unit -> Effect Unit)
  -> DelegateWebSocketServer
  -> (AppExitReason -> Effect Unit)
  -> Effect Unit
appCleanupHandler closeHttpServer wsServer cleanupAppInstances = do
  log "Stopping HTTP server."
    *> closeHttpServer (pure unit)
  log "Stopping WebSocket server."
    *> launchAff_ wsServer.close
  log "Executing app instance cleanup handlers."
    *> cleanupAppInstances AppExitReason_Cleanup

appInstanceCleanupHandler
  :: ChildProcess
  -> HydraNodeApiWebSocket
  -> ContractEnv
  -> Array (AVar (Maybe TimeoutId))
  -> Effect Unit
appInstanceCleanupHandler hydraNodeProcess hydraNodeApiWs contractEnv timers = do
  log "Cancelling timers."
    *> cancelTimers timers
  log "Closing Hydra Node API WebSocket connection."
    *> hydraNodeApiWs.baseWs.close
  log "Stopping hydra-node."
    *> kill SIGTERM hydraNodeProcess
  log "Finalizing Contract environment."
    *> launchAff_ (stopContractEnv contractEnv)

cancelTimers :: Array (AVar (Maybe TimeoutId)) -> Effect Unit
cancelTimers =
  traverse_
    ( \timer ->
        AVarSync.take timer \timeout ->
          traverse_ (traverse_ Timer.clearTimeout) timeout
    )
