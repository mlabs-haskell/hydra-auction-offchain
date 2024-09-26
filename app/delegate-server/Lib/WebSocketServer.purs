module DelegateServer.Lib.WebSocketServer
  ( WebSocketCloseReason
  , WebSocketOnConnect
  , WebSocketOnConnectAction(AcceptWebSocketConn, CloseWebSocketConn)
  , WebSocketOnConnectCb
  , WebSocketServer
  , WebSocketServerOptions
  , mkWebSocketServer
  ) where

import Prelude

import Contract.Value (CurrencySymbol)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(Right))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (class MonadEffect, liftEffect)
import HydraAuctionOffchain.Helpers (csToHex)
import HydraAuctionOffchain.Lib.Json (caEncodeString)

foreign import data WebSocketServerObj :: Type

foreign import data WebSocketConnection :: Type

foreign import newWebSocketServer :: WebSocketServerOptions -> Effect WebSocketServerObj

foreign import sendMessage :: WebSocketConnection -> String -> Effect Unit

foreign import closeConn :: WebSocketConnection -> Int -> String -> Effect Unit

foreign import broadcastMessage :: WebSocketServerObj -> String -> String -> Effect Unit

foreign import closeWebSocketServer :: WebSocketServerObj -> Effect Unit -> Effect Unit

foreign import onConnect
  :: WebSocketServerObj
  -> (WebSocketConnection -> String -> Effect Unit)
  -> Effect Unit

type WebSocketServerOptions =
  { host :: String
  , port :: Int
  }

type WebSocketServer out =
  { onConnect :: WebSocketOnConnectCb out -> Effect Unit
  , broadcast :: CurrencySymbol -> out -> Effect Unit
  , close :: Aff Unit
  }

type WebSocketOnConnectCb out = WebSocketOnConnect out -> Effect WebSocketOnConnectAction

type WebSocketOnConnect out =
  { connPath :: String
  , sendMessages :: Array out -> Effect Unit
  }

data WebSocketOnConnectAction
  = AcceptWebSocketConn
  | CloseWebSocketConn WebSocketCloseReason

type WebSocketCloseReason =
  { code :: Int
  , reason :: String
  }

mkWebSocketServer
  :: forall m out
   . MonadEffect m
  => CA.JsonCodec out
  -> WebSocketServerOptions
  -> m (WebSocketServer out)
mkWebSocketServer outMessageCodec options = do
  wss <- liftEffect $ newWebSocketServer options
  pure
    { onConnect: \cb ->
        onConnect wss \conn connPath -> do
          action <- cb
            { connPath
            , sendMessages: traverse_ (sendMessage conn <<< caEncodeString outMessageCodec)
            }
          case action of
            CloseWebSocketConn { code, reason } -> closeConn conn code reason
            AcceptWebSocketConn -> pure unit

    , broadcast: \auctionCs ->
        broadcastMessage wss (csToHex auctionCs)
          <<< caEncodeString outMessageCodec

    , close:
        makeAff \cb -> do
          closeWebSocketServer wss $ cb (Right unit)
          pure nonCanceler
    }
