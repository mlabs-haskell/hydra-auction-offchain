module Test.Plutip.Config
  ( plutipConfig
  ) where

import Prelude

import Contract.Config (LogLevel(Trace), emptyHooks)
import Contract.Test.Plutip (PlutipConfig)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt

plutipConfig :: PlutipConfig
plutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
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
      { slotLength: Seconds 0.1
      , epochSize: Just $ UInt.fromInt 4320000
      , maxTxSize: Just $ UInt.fromInt 16384
      , raiseExUnitsToMax: false
      }
  }
