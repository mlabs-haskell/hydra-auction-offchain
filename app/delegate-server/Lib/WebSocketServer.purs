module DelegateServer.Lib.WebSocketServer
  ( WebSocketServer
  , WebSocketServerOptions
  , mkWebSocketServer
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(Right))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import HydraAuctionOffchain.Lib.Json (caEncodeString)

foreign import data WebSocketServerObj :: Type
foreign import newWebSocketServer :: WebSocketServerOptions -> Effect WebSocketServerObj
foreign import broadcastMessage :: WebSocketServerObj -> String -> Effect Unit
foreign import closeWebSocketServer :: WebSocketServerObj -> Effect Unit -> Effect Unit

type WebSocketServerOptions =
  { host :: String
  , port :: Int
  }

type WebSocketServer (out :: Type) =
  { broadcast :: out -> Effect Unit
  , close :: Aff Unit
  }

mkWebSocketServer
  :: forall (out :: Type)
   . CA.JsonCodec out
  -> WebSocketServerOptions
  -> Effect (WebSocketServer out)
mkWebSocketServer outMessageCodec options = do
  wss <- newWebSocketServer options
  pure
    { broadcast:
        broadcastMessage wss <<< caEncodeString outMessageCodec
    , close:
        makeAff \cb -> do
          closeWebSocketServer wss $ cb (Right unit)
          pure nonCanceler
    }
