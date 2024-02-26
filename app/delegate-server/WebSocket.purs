module DelegateServer.WebSocket
  ( mkWebSocket
  ) where

import Prelude

import Control.Monad.Base (class MonadBase, liftBase)
import Ctl.Internal.JsWebSocket (_mkWebSocket, _onWsConnect, _onWsMessage, _wsSend)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (either)
import Data.UInt (toString) as UInt
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import HydraAuctionOffchain.Config (HostPort)
import HydraAuctionOffchain.Lib.Json (caDecodeString, caEncodeString)

type WebSocket (m :: Type -> Type) (msgReceive :: Type) (msgSend :: Type) =
  { onConnect :: m Unit -> m Unit
  , onMessage :: (msgReceive -> m Unit) -> m Unit
  , send :: msgSend -> m Unit
  }

mkWebSocket
  :: forall (m :: Type -> Type) (msgReceive :: Type) (msgSend :: Type)
   . MonadBase m Effect
  => MonadEffect m
  => HostPort
  -> CA.JsonCodec msgReceive
  -> CA.JsonCodec msgSend
  -> m (WebSocket m msgReceive msgSend)
mkWebSocket hostPort msgReceiveCodec msgSendCodec = do
  ws <- liftEffect $ _mkWebSocket wsLogger $ mkWsUrl hostPort
  pure
    { onConnect:
        \callback ->
          liftEffect $
            _onWsConnect ws (liftBase callback)

    , onMessage:
        \callback ->
          liftEffect $
            _onWsMessage ws wsLogger \msgRaw ->
              either (\_ -> pure unit) (liftBase <<< callback) $ caDecodeString msgReceiveCodec
                msgRaw

    , send:
        \msg ->
          liftEffect $
            _wsSend ws wsLogger (caEncodeString msgSendCodec msg)
    }
  where
  wsLogger :: String -> Effect Unit
  wsLogger _ = pure unit

mkWsUrl :: HostPort -> String
mkWsUrl rec = "ws://" <> rec.host <> ":" <> UInt.toString rec.port
