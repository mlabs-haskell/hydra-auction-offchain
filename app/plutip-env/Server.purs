module PlutipEnv.Server
  ( server
  ) where

import Prelude

import Aeson (encodeAeson, stringifyAeson)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile)
import Ctl.Internal.ServerConfig (mkHttpUrl)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Data.UInt (toInt) as UInt
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import HTTPure (Method(Get, Options), (!?), (!@))
import HTTPure as HTTPure
import HTTPure.Status (ok) as HTTPureStatus
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

demoOrigin :: String
demoOrigin = mkHttpUrl Config.demoServerConfig

corsPreflightHandler :: HTTPure.Headers -> Aff HTTPure.Response
corsPreflightHandler headers =
  HTTPure.emptyResponse' HTTPureStatus.ok $
    HTTPure.headers
      [ "Access-Control-Allow-Origin" /\ demoOrigin
      , "Access-Control-Allow-Methods" /\ (headers !@ "Access-Control-Request-Method")
      , "Access-Control-Allow-Headers" /\ (headers !@ "Access-Control-Request-Headers")
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
            HTTPure.header "Access-Control-Allow-Origin" demoOrigin
      }
