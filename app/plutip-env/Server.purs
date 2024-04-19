module PlutipEnv.Server
  ( server
  ) where

import Prelude

import Aeson (encodeAeson, stringifyAeson)
import Contract.Config (ServerConfig)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile)
import Control.Error.Util (bool)
import Ctl.Internal.ServerConfig (mkHttpUrl)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.String (Pattern(Pattern))
import Data.String (stripPrefix) as String
import Data.Tuple.Nested ((/\))
import Data.UInt (toInt) as UInt
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import HTTPure (Method(Get, Options), (!?), (!@))
import HTTPure as HTTPure
import HTTPure.Status (ok) as HTTPureStatus
import HydraAuctionOffchain.Types.HostPort (HostPort, readHostPort)
import PlutipEnv.Config (PlutipEnvConfig)

server :: PlutipEnvConfig -> HTTPure.ServerM
server config = do
  let port = UInt.toInt config.plutipEnvHostPort.port
  HTTPure.serve port (router config) do
    log $ "plutip-env server now accepts connections on port " <> show port <> "."

router :: PlutipEnvConfig -> HTTPure.Request -> Aff HTTPure.Response
router config { method: Options, headers }
  | unwrap headers !? "Access-Control-Request-Method" = corsPreflightHandler config headers
  | otherwise = HTTPure.notFound

router config request = corsMiddleware config (routerCors config) request

routerCors :: PlutipEnvConfig -> HTTPure.Request -> Aff HTTPure.Response
routerCors config { method: Get, path: [ "skey" ] } = do
  paymentSkey <- privatePaymentKeyFromFile config.paymentSkeyFilePath
  HTTPure.ok $ stringifyAeson $ encodeAeson paymentSkey

routerCors _ _ = HTTPure.notFound

demoOrigin :: PlutipEnvConfig -> HTTPure.Headers -> String
demoOrigin config reqHeaders =
  case origin of
    Just { host } | localhost host == localhost demoServerConfig.host ->
      mkHttpUrl $ demoServerConfig { host = host }
    _ ->
      mkHttpUrl demoServerConfig
  where
  origin :: Maybe HostPort
  origin = readHostPort =<< String.stripPrefix (Pattern "http://") (reqHeaders !@ "Origin")

  localhost :: String -> String
  localhost host = bool host "127.0.0.1" $ host == "localhost"

  demoServerConfig :: ServerConfig
  demoServerConfig =
    { port: config.demoHostPort.port
    , host: config.demoHostPort.host
    , secure: false
    , path: Nothing
    }

corsPreflightHandler :: PlutipEnvConfig -> HTTPure.Headers -> Aff HTTPure.Response
corsPreflightHandler config reqHeaders =
  HTTPure.emptyResponse' HTTPureStatus.ok $
    HTTPure.headers
      [ "Access-Control-Allow-Origin" /\ demoOrigin config reqHeaders
      , "Access-Control-Allow-Methods" /\ (reqHeaders !@ "Access-Control-Request-Method")
      , "Access-Control-Allow-Headers" /\ (reqHeaders !@ "Access-Control-Request-Headers")
      ]

corsMiddleware
  :: PlutipEnvConfig
  -> (HTTPure.Request -> Aff HTTPure.Response)
  -> HTTPure.Request
  -> Aff HTTPure.Response
corsMiddleware config router' request =
  router' request <#> \response ->
    response
      { headers =
          response.headers <>
            HTTPure.header "Access-Control-Allow-Origin" (demoOrigin config request.headers)
      }
