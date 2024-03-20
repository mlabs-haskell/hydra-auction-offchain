module DelegateServer.Server
  ( server
  ) where

import Prelude

import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import DelegateServer.App (AppState, AppM, runApp)
import DelegateServer.Handlers.MoveBid (moveBidHandler)
import DelegateServer.Handlers.PlaceBid (placeBidHandler)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import HTTPure
  ( Headers
  , Request
  , Response
  , ServerM
  , emptyResponse'
  , header
  , headers
  , notFound
  , serve
  , toString
  ) as HTTPure
import HTTPure (Method(Options, Post), (!?), (!@))
import HTTPure.Status (ok) as HTTPureStatus
import URI.Port (toInt) as Port

server :: AppState -> HydraNodeApiWebSocket -> HTTPure.ServerM
server appState ws = do
  let port = Port.toInt appState.config.serverPort
  HTTPure.serve port (runApp appState <<< router ws) do
    log $ "Http server now accepts connections on port " <> show port <> "."

router :: HydraNodeApiWebSocket -> HTTPure.Request -> AppM HTTPure.Response
router _ { method: Options, headers }
  | unwrap headers !? "Access-Control-Request-Method" = corsPreflightHandler headers
  | otherwise = HTTPure.notFound

router ws request = corsMiddleware routerCors ws request

routerCors :: HydraNodeApiWebSocket -> HTTPure.Request -> AppM HTTPure.Response
routerCors ws { method: Post, path: [ "moveBid" ] } =
  moveBidHandler ws

routerCors ws { body, method: Post, path: [ "placeBid" ] } = do
  bodyStr <- liftAff $ HTTPure.toString body
  placeBidHandler ws bodyStr

routerCors _ _ = HTTPure.notFound

-- Middleware --------------------------------------------------------

corsMiddleware
  :: (HydraNodeApiWebSocket -> HTTPure.Request -> AppM HTTPure.Response)
  -> HydraNodeApiWebSocket
  -> HTTPure.Request
  -> AppM HTTPure.Response
corsMiddleware router' ws request =
  router' ws request <#> \response ->
    response
      { headers =
          response.headers <>
            HTTPure.header "Access-Control-Allow-Origin" "*"
      }

corsPreflightHandler :: HTTPure.Headers -> AppM HTTPure.Response
corsPreflightHandler headers =
  HTTPure.emptyResponse' HTTPureStatus.ok $
    HTTPure.headers
      [ "Access-Control-Allow-Origin" /\ "*"
      , "Access-Control-Allow-Methods" /\ (headers !@ "Access-Control-Request-Method")
      , "Access-Control-Allow-Headers" /\ (headers !@ "Access-Control-Request-Headers")
      ]