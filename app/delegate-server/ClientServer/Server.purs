module DelegateServer.ClientServer.Server
  ( clientServer
  ) where

import Prelude

import DelegateServer.ClientServer.Handlers.MoveBid (moveBidHandler)
import DelegateServer.ClientServer.Handlers.PlaceBid (placeBidHandler)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (AppState, AppM, runApp)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import HTTPure (Method(Post))
import HTTPure (Request, Response, ServerM, notFound, serve, toString) as HTTPure
import URI.Port (toInt) as Port

clientServer :: AppState -> HydraNodeApiWebSocket -> HTTPure.ServerM
clientServer appState ws = do
  let port = Port.toInt appState.config.clientServerPort
  HTTPure.serve port (runApp appState <<< router ws) do
    log $ "Client server now accepts connections on port " <> show port <> "."

router :: HydraNodeApiWebSocket -> HTTPure.Request -> AppM HTTPure.Response
router ws { method: Post, path: [ "moveBid" ] } =
  moveBidHandler ws

router ws { body, method: Post, path: [ "placeBid" ] } = do
  bodyStr <- liftAff $ HTTPure.toString body
  placeBidHandler ws bodyStr

router _ _ = HTTPure.notFound
