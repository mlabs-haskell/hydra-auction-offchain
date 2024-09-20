module DelegateServer.Server
  ( httpServer
  ) where

import Prelude

import Data.Either (Either(Left, Right))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import DelegateServer.App (AppM, runApp)
import DelegateServer.AppManager.Types (AppManager')
import DelegateServer.Handlers.MoveBid (moveBidHandler)
import DelegateServer.Handlers.PlaceBid (placeBidHandler)
import DelegateServer.Handlers.SignCommitTx (signCommitTxHandler)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (read) as AVar
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

type AppManager (wsServer :: Type) = AppManager' HydraNodeApiWebSocket wsServer

httpServer :: forall a. Port -> AVar (AppManager a) -> HTTPure.ServerM
httpServer serverPort appManagerAvar = do
  let port = Port.toInt serverPort
  HTTPure.serve port (router appManagerAvar) do
    -- TODO: Add a top-level logger that is not associated with a
    -- specific app instance   
    log $ "Http server now accepts connections on port " <> show port <> "."

router :: forall a. AVar (AppManager a) -> HTTPure.Request -> Aff HTTPure.Response
router _ { method: Options, headers }
  | unwrap headers !? "Access-Control-Request-Method" = corsPreflightHandler headers
  | otherwise = HTTPure.notFound

router appManagerAvar request = corsMiddleware routerCors appManagerAvar request

routerCors :: forall a. AVar (AppManager a) -> HTTPure.Request -> Aff HTTPure.Response
routerCors appManagerAvar request = appLookupMiddleware routerCorsApp appManagerAvar request

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
  :: forall a
   . (HydraNodeApiWebSocket -> HTTPure.Request -> AppM HTTPure.Response)
  -> AVar (AppManager a)
  -> HTTPure.Request
  -> Aff HTTPure.Response
appLookupMiddleware router' appManagerAvar request@{ headers }
  | Just auctionCsRaw <- headers !! "Auction-Cs" =
      case caDecodeString scriptHashCodec auctionCsRaw of
        Left _ ->
          HTTPure.badRequest "Could not decode Auction-Cs request header"
        Right auctionCs -> do
          { activeAuctions } <- unwrap <$> AVar.read appManagerAvar
          case Map.lookup auctionCs activeAuctions of
            Nothing ->
              HTTPure.badRequest
                "Provided Auction-Cs does not correspond to any auction served by this \
                \delegate server"
            Just { appState, appLogger, hydraNodeApiWs } ->
              runApp appState appLogger $ router' hydraNodeApiWs request
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
