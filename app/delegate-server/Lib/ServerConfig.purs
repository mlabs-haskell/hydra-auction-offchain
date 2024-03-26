module DelegateServer.Lib.ServerConfig
  ( mkLocalhostHttpServerConfig
  ) where

import Contract.Config (ServerConfig)
import Data.Maybe (Maybe(Nothing))
import Data.UInt (UInt)

mkLocalhostHttpServerConfig :: UInt -> ServerConfig
mkLocalhostHttpServerConfig port =
  { port
  , host: "127.0.0.1"
  , secure: false
  , path: Nothing
  }
