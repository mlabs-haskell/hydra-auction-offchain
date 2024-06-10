module DelegateServer.Lib.WebSocketServer
  ( WebSocketServer
  , WebSocketServerOptions
  , mkWebSocketServer
  ) where

import Prelude

import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Value (CurrencySymbol, getCurrencySymbol)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (Either(Right))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (class MonadEffect, liftEffect)
import HydraAuctionOffchain.Lib.Json (caEncodeString)

foreign import data WebSocketServerObj :: Type

foreign import data WebSocketConnection :: Type

foreign import newWebSocketServer :: WebSocketServerOptions -> Effect WebSocketServerObj

foreign import sendMessage :: WebSocketConnection -> String -> Effect Unit

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

type WebSocketServer (out :: Type) =
  { onConnect :: (String -> (Array out -> Effect Unit) -> Effect Unit) -> Effect Unit
  , broadcast :: CurrencySymbol -> out -> Effect Unit
  , close :: Aff Unit
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
        onConnect wss \conn path ->
          cb path (traverse_ (sendMessage conn <<< caEncodeString outMessageCodec))

    , broadcast: \auctionCs message ->
        broadcastMessage wss (byteArrayToHex $ getCurrencySymbol auctionCs)
          (caEncodeString outMessageCodec message)

    , close:
        makeAff \cb -> do
          closeWebSocketServer wss $ cb (Right unit)
          pure nonCanceler
    }
