module DelegateServer.WsServer
  ( DelegateWebSocketServer
  , DelegateWebSocketServerMessage
      ( HydraHeadStatus
      , StandingBid
      )
  , wsServer
  ) where

import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right))
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Variant (inj, match) as Variant
import DelegateServer.Config (AppConfig)
import DelegateServer.Lib.WebSocketServer (WebSocketServer, mkWebSocketServer)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus, headStatusCodec)
import Effect (Effect)
import HydraAuctionOffchain.Contract.Types (StandingBidState, standingBidStateCodec)
import Type.Proxy (Proxy(Proxy))
import URI.Port (toInt) as Port

wsServer :: AppConfig -> Effect DelegateWebSocketServer
wsServer appConfig =
  mkWebSocketServer delegateWsServerMessageCodec
    { host: "0.0.0.0"
    , port: Port.toInt (unwrap appConfig).wsServerPort
    }

type DelegateWebSocketServer = WebSocketServer DelegateWebSocketServerMessage

data DelegateWebSocketServerMessage
  = HydraHeadStatus HydraHeadStatus
  | StandingBid StandingBidState

delegateWsServerMessageCodec :: CA.JsonCodec DelegateWebSocketServerMessage
delegateWsServerMessageCodec =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "HydraHeadStatus": Right headStatusCodec
        , "StandingBid": Right standingBidStateCodec
        }
    )
  where
  toVariant = case _ of
    HydraHeadStatus x ->
      Variant.inj (Proxy :: _ "HydraHeadStatus") x
    StandingBid x ->
      Variant.inj (Proxy :: _ "StandingBid") x

  fromVariant = Variant.match
    { "HydraHeadStatus": HydraHeadStatus
    , "StandingBid": StandingBid
    }
