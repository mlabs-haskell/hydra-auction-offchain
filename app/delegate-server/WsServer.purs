module DelegateServer.WsServer
  ( DelegateWebSocketServer
  , wsServer
  ) where

import Data.Newtype (unwrap)
import DelegateServer.Config (AppConfig)
import DelegateServer.Lib.WebSocketServer (WebSocketServer, mkWebSocketServer)
import Effect (Effect)
import HydraAuctionOffchain.Contract.Types (StandingBidState, standingBidStateCodec)
import URI.Port (toInt) as Port

type DelegateWebSocketServer = WebSocketServer StandingBidState

wsServer :: AppConfig -> Effect DelegateWebSocketServer
wsServer appConfig =
  mkWebSocketServer standingBidStateCodec
    { host: "0.0.0.0"
    , port: Port.toInt (unwrap appConfig).wsServerPort
    }

