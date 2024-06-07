module DelegateServer.WsServer
  ( DelegateWebSocketServer
  , DelegateWebSocketServerMessage
      ( HydraHeadStatus
      , StandingBid
      )
  , wsServer
  ) where

import Prelude

import Contract.Address (getNetworkId)
import Contract.Config (NetworkId)
import Data.Array (singleton, (:))
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right))
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Tuple (snd)
import Data.Variant (inj, match) as Variant
import DelegateServer.App (AppM, getAppEffRunner, runContract)
import DelegateServer.AppMap (AppMap)
import DelegateServer.Config (AppConfig, Network, networkToNetworkId)
import DelegateServer.Contract.StandingBid (queryStandingBidL2)
import DelegateServer.Lib.WebSocketServer
  ( WebSocketServer
  , WebSocketServerOptions
  , mkWebSocketServer
  )
import DelegateServer.State (readAppState)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus, headStatusCodec)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Contract.Types (StandingBidState, standingBidStateCodec)
import Type.Proxy (Proxy(Proxy))
import URI.Port (Port)
import URI.Port (toInt) as Port

wsServer :: Port -> Network -> AppMap Unit -> Aff DelegateWebSocketServer
wsServer wsServerPort network appMap = do
  wss <- mkWebSocketServer (delegateWsServerMessageCodec $ networkToNetworkId network)
    wsServerOptions
  liftEffect do
    wss.onConnect \path sendMessages ->
      runAppEff' do
        headStatus <- readAppState (Proxy :: _ "headStatus")
        standingBid <- map snd <$> queryStandingBidL2
        liftEffect $ sendMessages $
          HydraHeadStatus headStatus
            : maybe mempty (singleton <<< StandingBid) standingBid
  pure wss
  where
  wsServerOptions :: WebSocketServerOptions
  wsServerOptions =
    { host: "0.0.0.0"
    , port: Port.toInt wsServerPort
    }

type DelegateWebSocketServer = WebSocketServer DelegateWebSocketServerMessage

data DelegateWebSocketServerMessage
  = HydraHeadStatus HydraHeadStatus
  | StandingBid StandingBidState

delegateWsServerMessageCodec :: NetworkId -> CA.JsonCodec DelegateWebSocketServerMessage
delegateWsServerMessageCodec network =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "HydraHeadStatus": Right headStatusCodec
        , "StandingBid": Right $ standingBidStateCodec network
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
