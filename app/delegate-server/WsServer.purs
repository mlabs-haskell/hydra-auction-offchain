module DelegateServer.WsServer
  ( DelegateWebSocketServer
  , DelegateWebSocketServerMessage
      ( HydraHeadStatus
      , StandingBid
      )
  , wsServer
  ) where

import Prelude

import Data.Array (singleton, (:))
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right))
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Tuple (snd)
import Data.Variant (inj, match) as Variant
import DelegateServer.App (AppM, getAppEffRunner)
import DelegateServer.Config (AppConfig)
import DelegateServer.Contract.StandingBid (queryStandingBidL2)
import DelegateServer.Lib.WebSocketServer
  ( WebSocketServer
  , WebSocketServerOptions
  , mkWebSocketServer
  )
import DelegateServer.State (readAppState)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus, headStatusCodec)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Contract.Types (StandingBidState, standingBidStateCodec)
import Type.Proxy (Proxy(Proxy))
import URI.Port (toInt) as Port

wsServer :: AppConfig -> AppM DelegateWebSocketServer
wsServer appConfig = do
  wss <- mkWebSocketServer delegateWsServerMessageCodec wsServerOptions
  runAppEff' <- getAppEffRunner
  liftEffect do
    wss.onConnect \sendMessages ->
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
