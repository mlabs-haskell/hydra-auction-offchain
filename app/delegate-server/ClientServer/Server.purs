module DelegateServer.ClientServer.Server
  ( clientServer
  ) where

import Prelude

import DelegateServer.ClientServer.Handlers.MoveBidL2 (moveBidL2Handler)
import DelegateServer.State (AppState, AppM, runApp)
import Effect.Console (log)
import HTTPure (Method(Post))
import HTTPure (Request, Response, ServerM, notFound, serve) as HTTPure
import URI.Port (toInt) as Port

clientServer :: AppState -> HTTPure.ServerM
clientServer appState = do
  let port = Port.toInt appState.config.clientServerPort
  HTTPure.serve port (runApp appState <<< router) do
    log $ "Client server now accepts connections on port " <> show port <> "."

router :: HTTPure.Request -> AppM HTTPure.Response
router { method: Post, path: [ "moveBidL2" ] } =
  moveBidL2Handler

router _ = HTTPure.notFound
