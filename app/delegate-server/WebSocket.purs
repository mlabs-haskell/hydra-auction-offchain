module DelegateServer.WebSocket
  ( WebSocket
  , WebSocketBuilder
  , mkWebSocket
  , mkWsUrl
  ) where

import Prelude

import Ctl.Internal.JsWebSocket
  ( _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _wsClose
  , _wsFinalize
  , _wsSend
  )
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (either)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\))
import Data.UInt (toString) as UInt
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import HydraAuctionOffchain.Config (HostPort)
import HydraAuctionOffchain.Lib.Json (caDecodeString, caEncodeString)

type WebSocket (m :: Type -> Type) (in_ :: Type) (out :: Type) =
  { onConnect :: m Unit -> m Unit
  , onMessage :: (in_ -> m Unit) -> m Unit
  , onError :: (String -> m Unit) -> m Unit
  , send :: out -> Effect Unit
  , close :: Effect Unit
  }

type WebSocketBuilder (m :: Type -> Type) (in_ :: Type) (out :: Type) =
  { hostPort :: HostPort
  , inMsgCodec :: CA.JsonCodec in_
  , outMsgCodec :: CA.JsonCodec out
  , runM :: forall (a :: Type). m a -> Effect Unit
  }

mkWebSocket
  :: forall (m :: Type -> Type) (in_ :: Type) (out :: Type)
   . MonadEffect m
  => WebSocketBuilder m in_ out
  -> m (WebSocket m in_ out /\ String)
mkWebSocket builder = do
  ws <- liftEffect $ _mkWebSocket wsLogger wsUrl
  pure $ flip Tuple wsUrl $
    { onConnect:
        \callback ->
          liftEffect $
            _onWsConnect ws (builder.runM callback)

    , onMessage:
        \callback ->
          liftEffect $
            _onWsMessage ws wsLogger \msgRaw -> do
              log $ "_onWsMessage: " <> msgRaw
              either (\_ -> pure unit) (builder.runM <<< callback) $ caDecodeString
                builder.inMsgCodec
                msgRaw

    , onError:
        \callback ->
          void $ liftEffect $
            _onWsError ws (builder.runM <<< callback)

    , send:
        \msg ->
          _wsSend ws wsLogger (caEncodeString builder.outMsgCodec msg)

    , close:
        _wsFinalize ws *> _wsClose ws
    }
  where
  wsUrl :: String
  wsUrl = mkWsUrl builder.hostPort

  wsLogger :: String -> Effect Unit
  wsLogger _ = pure unit

mkWsUrl :: HostPort -> String
mkWsUrl rec = "ws://" <> rec.host <> ":" <> UInt.toString rec.port
