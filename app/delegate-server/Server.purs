module DelegateServer.Server
  ( httpServer
  ) where

import Prelude

import Data.Either (Either(Left, Right))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import DelegateServer.App (AppLogger, AppM, runApp)
import DelegateServer.AppMap (AppMap)
import DelegateServer.Handlers.MoveBid (moveBidHandler)
import DelegateServer.Handlers.PlaceBid (placeBidHandler)
import DelegateServer.Handlers.SignCommitTx (signCommitTxHandler)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import HTTPure
  ( Headers
  , Request
  , Response
  , ServerM
  , badRequest
  , emptyResponse'
  , header
  , headers
  , notFound
  , serve
  , toString
  ) as HTTPure
import HTTPure (Method(Options, Post), (!!), (!?), (!@))
import HTTPure.Status (ok) as HTTPureStatus
import HydraAuctionOffchain.Codec (scriptHashCodec)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import URI.Port (Port)
import URI.Port (toInt) as Port

type AppMap' = AppMap HydraNodeApiWebSocket

httpServer :: Port -> AppLogger -> AppMap' -> HTTPure.ServerM
httpServer serverPort appLogger appMap = do
  let port = Port.toInt serverPort
  HTTPure.serve port (router (appLogger /\ appMap)) do
    -- TODO: Add a top-level logger that is not associated with a
    -- specific app instance   
    log $ "Http server now accepts connections on port " <> show port <> "."

router :: AppLogger /\ AppMap' -> HTTPure.Request -> Aff HTTPure.Response
router _ { method: Options, headers }
  | unwrap headers !? "Access-Control-Request-Method" = corsPreflightHandler headers
  | otherwise = HTTPure.notFound

router appMap request = corsMiddleware routerCors appMap request

routerCors :: AppLogger /\ AppMap' -> HTTPure.Request -> Aff HTTPure.Response
routerCors appMap request = appLookupMiddleware routerCorsApp appMap request

routerCorsApp :: HydraNodeApiWebSocket -> HTTPure.Request -> AppM HTTPure.Response
routerCorsApp ws { method: Post, path: [ "moveBid" ] } =
  moveBidHandler ws

routerCorsApp ws { body, method: Post, path: [ "placeBid" ] } = do
  bodyStr <- liftAff $ HTTPure.toString body
  placeBidHandler ws bodyStr

routerCorsApp _ { body, method: Post, path: [ "signCommitTx" ] } = do
  bodyStr <- liftAff $ HTTPure.toString body
  signCommitTxHandler bodyStr

routerCorsApp _ _ = HTTPure.notFound

-- Middleware --------------------------------------------------------

appLookupMiddleware
  :: (HydraNodeApiWebSocket -> HTTPure.Request -> AppM HTTPure.Response)
  -> AppLogger /\ AppMap'
  -> HTTPure.Request
  -> Aff HTTPure.Response
appLookupMiddleware router' (appLogger /\ appMap) request@{ headers }
  | Just auctionCsRaw <- headers !! "Auction-Cs" =
      case caDecodeString scriptHashCodec auctionCsRaw of
        Left _ ->
          HTTPure.badRequest "Could not decode Auction-Cs request header"
        Right auctionCs ->
          case Map.lookup auctionCs appMap of
            Nothing ->
              HTTPure.badRequest
                "Provided Auction-Cs does not correspond to any auction served by this \
                \delegate server"
            Just (appState /\ ws) ->
              runApp appState appLogger $ router' ws request
  | otherwise =
      HTTPure.badRequest "Missing Auction-Cs request header"

corsMiddleware
  :: forall a
   . (a -> HTTPure.Request -> Aff HTTPure.Response)
  -> a
  -> HTTPure.Request
  -> Aff HTTPure.Response
corsMiddleware router' params request =
  router' params request <#> \response ->
    response
      { headers =
          response.headers <>
            HTTPure.header "Access-Control-Allow-Origin" "*"
      }

corsPreflightHandler :: HTTPure.Headers -> Aff HTTPure.Response
corsPreflightHandler headers =
  HTTPure.emptyResponse' HTTPureStatus.ok $
    HTTPure.headers
      [ "Access-Control-Allow-Origin" /\ "*"
      , "Access-Control-Allow-Methods" /\ (headers !@ "Access-Control-Request-Method")
      , "Access-Control-Allow-Headers" /\ (headers !@ "Access-Control-Request-Headers")
      ]
