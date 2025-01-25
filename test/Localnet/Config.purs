module Test.Localnet.Config
  ( localnetConfig
  ) where

import Prelude

import Contract.Config (LogLevel(Trace), emptyHooks)
import Contract.Test.Testnet (Era(Conway), TestnetConfig)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt

localnetConfig :: TestnetConfig
localnetConfig =
  { logLevel: Trace
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , suppressLogs: true
  , customLogger: Just $ \_ _ -> pure unit
  , hooks: emptyHooks
  , clusterConfig:
      { testnetMagic: 2
      , era: Conway
      , slotLength: Seconds 0.1
      , epochSize: Just $ UInt.fromInt 4320000
      }
  }
