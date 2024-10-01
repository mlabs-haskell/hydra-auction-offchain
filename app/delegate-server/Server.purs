module DelegateServer.Server
  ( httpServer
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Contract.CborBytes (hexToCborBytes)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds)
import Data.Tuple.Nested ((/\))
import DelegateServer.App (AppM, runApp)
import DelegateServer.AppManager (AppManager)
import DelegateServer.Handlers.GetAvailableSlots (getAvailableSlotsHandler)
import DelegateServer.Handlers.HostAuction (hostAuctionHandler)
import DelegateServer.Handlers.MoveBid (moveBidHandler)
import DelegateServer.Handlers.PlaceBid (placeBidHandler)
import DelegateServer.Handlers.ReserveSlot (reserveSlotHandler)
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
import HTTPure (Method(Get, Options, Post), (!!), (!?), (!@))
import HTTPure.Status (ok) as HTTPureStatus
import URI.Port (Port)
import URI.Port (toInt) as Port

type HttpServerParams =
  { serverPort :: Port
  , appManagerAvar :: AVar AppManager
  , slotReservationPeriod :: Seconds
  }

httpServer :: HttpServerParams -> HTTPure.ServerM
httpServer params = do
  let port = Port.toInt params.serverPort
  HTTPure.serve port (router params) do
    -- TODO: Add a top-level logger that is not associated with a
    -- specific app instance   
    log $ "Http server now accepts connections on port " <> show port <> "."

router :: HttpServerParams -> HTTPure.Request -> Aff HTTPure.Response
router _ { method: Options, headers }
  | unwrap headers !? "Access-Control-Request-Method" = corsPreflightHandler headers
  | otherwise = HTTPure.notFound

router params request = corsMiddleware routerCors params request

routerCors :: HttpServerParams -> HTTPure.Request -> Aff HTTPure.Response
routerCors params { method: Get, path: [ "availableSlots" ] } =
  getAvailableSlotsHandler params.appManagerAvar

routerCors params { body, method: Post, path: [ "reserveSlot" ] } = do
  bodyStr <- liftAff $ HTTPure.toString body
  reserveSlotHandler params.appManagerAvar params.slotReservationPeriod
    bodyStr

routerCors params { body, method: Post, path: [ "hostAuction" ] } = do
  bodyStr <- liftAff $ HTTPure.toString body
  hostAuctionHandler params.appManagerAvar bodyStr

routerCors params request =
  appLookupMiddleware routerCorsApp params.appManagerAvar request

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
  -> AVar AppManager
  -> HTTPure.Request
  -> Aff HTTPure.Response
appLookupMiddleware router' appManagerAvar request@{ headers }
  | Just auctionCsRaw <- headers !! "Auction-Cs" =
      case decodeCbor =<< hexToCborBytes auctionCsRaw of
        Nothing ->
          HTTPure.badRequest "Could not decode Auction-Cs request header"
        Just auctionCs -> do
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
