module DelegateServer.WebSocket
  ( WebSocket
  , WebSocketBuilder
  , mkWebSocket
  , mkWsUrl
  ) where

import Prelude

import Contract.Log (logTrace')
import Control.Monad.Logger.Class (class MonadLogger)
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
import Effect.Class (class MonadEffect)
import HydraAuctionOffchain.Lib.Json (caDecodeString, caEncodeString)
import HydraAuctionOffchain.Types.HostPort (HostPort)

type WebSocket (m :: Type -> Type) (in_ :: Type) (out :: Type) =
  { onConnect :: m Unit -> Effect Unit
  , onMessage :: (in_ -> m Unit) -> Effect Unit
  , onError :: (String -> m Unit) -> Effect Unit
  , send :: out -> Effect Unit
  , close :: Effect Unit
  }

type WebSocketBuilder (m :: Type -> Type) (in_ :: Type) (out :: Type) =
  { hostPort :: HostPort
  , inMsgCodec :: CA.JsonCodec in_
  , outMsgCodec :: CA.JsonCodec out
  , runM :: forall a. m a -> Effect Unit
  }

mkWebSocket
  :: forall (m :: Type -> Type) (in_ :: Type) (out :: Type)
   . MonadEffect m
  => MonadLogger m
  => WebSocketBuilder m in_ out
  -> Effect (WebSocket m in_ out /\ String)
mkWebSocket builder = do
  ws <- _mkWebSocket wsLogger wsUrl
  pure $ flip Tuple wsUrl $
    { onConnect:
        \callback ->
          _onWsConnect ws (builder.runM callback)

    , onMessage:
        \callback ->
          _onWsMessage ws wsLogger \msgRaw ->
            builder.runM do
              logTrace' $ "onMessage raw: " <> msgRaw
              either (logTrace' <<< append "onMessage decode error: ") callback
                (caDecodeString builder.inMsgCodec msgRaw)

    , onError:
        \callback ->
          void $ _onWsError ws (builder.runM <<< callback)

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
