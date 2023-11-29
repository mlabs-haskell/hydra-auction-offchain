module PlutipEnv.Server
  ( server
  ) where

import Prelude

import Aeson (encodeAeson, stringifyAeson)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile)
import Control.Error.Util (bool)
import Ctl.Internal.ServerConfig (mkHttpUrl)
import Data.Maybe (Maybe(Just))
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
import HydraAuctionOffchain.Config (HostPort, readHostPort)
import HydraAuctionOffchain.Config (config, demoServerConfig) as Config
import PlutipEnv.Config (PlutipEnvConfig)

server :: PlutipEnvConfig -> HTTPure.ServerM
server config = do
  let port = UInt.toInt Config.config.plutipEnvHostPort.port
  HTTPure.serve port (router config) do
    log $ "plutip-env server now accepts connections on port " <> show port <> "."

router :: PlutipEnvConfig -> HTTPure.Request -> Aff HTTPure.Response
router _ { method: Options, headers }
  | unwrap headers !? "Access-Control-Request-Method" = corsPreflightHandler headers
  | otherwise = HTTPure.notFound

router config request = corsMiddleware (routerCors config) request

routerCors :: PlutipEnvConfig -> HTTPure.Request -> Aff HTTPure.Response
routerCors config { method: Get, path: [ "skey" ] } = do
  paymentSkey <- privatePaymentKeyFromFile config.paymentSkeyFilePath
  HTTPure.ok $ stringifyAeson $ encodeAeson paymentSkey

routerCors _ _ = HTTPure.notFound

demoOrigin :: HTTPure.Headers -> String
demoOrigin reqHeaders =
  case origin of
    Just { host } | localhost host == localhost Config.demoServerConfig.host ->
      mkHttpUrl $ Config.demoServerConfig { host = host }
    _ ->
      mkHttpUrl Config.demoServerConfig
  where
  origin :: Maybe HostPort
  origin = readHostPort =<< String.stripPrefix (Pattern "http://") (reqHeaders !@ "Origin")

  localhost :: String -> String
  localhost host = bool host "127.0.0.1" $ host == "localhost"

corsPreflightHandler :: HTTPure.Headers -> Aff HTTPure.Response
corsPreflightHandler reqHeaders =
  HTTPure.emptyResponse' HTTPureStatus.ok $
    HTTPure.headers
      [ "Access-Control-Allow-Origin" /\ demoOrigin reqHeaders
      , "Access-Control-Allow-Methods" /\ (reqHeaders !@ "Access-Control-Request-Method")
      , "Access-Control-Allow-Headers" /\ (reqHeaders !@ "Access-Control-Request-Headers")
      ]

corsMiddleware
  :: (HTTPure.Request -> Aff HTTPure.Response)
  -> HTTPure.Request
  -> Aff HTTPure.Response
corsMiddleware router' request =
  router' request <#> \response ->
    response
      { headers =
          response.headers <>
            HTTPure.header "Access-Control-Allow-Origin" (demoOrigin request.headers)
      }
