module DelegateServer.ClientServer.Server
  ( clientServer
  ) where

import Prelude

import DelegateServer.ClientServer.Handlers.MoveBidL2 (moveBidL2Handler)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (AppState, AppM, runApp)
import Effect.Console (log)
import HTTPure (Method(Post))
import HTTPure (Request, Response, ServerM, notFound, serve) as HTTPure
import URI.Port (toInt) as Port

clientServer :: AppState -> HydraNodeApiWebSocket -> HTTPure.ServerM
clientServer appState ws = do
  let port = Port.toInt appState.config.clientServerPort
  HTTPure.serve port (runApp appState <<< router ws) do
    log $ "Client server now accepts connections on port " <> show port <> "."

router :: HydraNodeApiWebSocket -> HTTPure.Request -> AppM HTTPure.Response
router ws { method: Post, path: [ "moveBidL2" ] } =
  moveBidL2Handler ws

router _ _ = HTTPure.notFound
