module Test.Main
  ( main
  ) where

import Prelude

import Contract.Config (LogLevel(Trace), emptyHooks)
import Contract.Test.Mote (TestPlanM, interpret)
import Contract.Test.Plutip (PlutipConfig, testPlutipContracts)
import Contract.Test.Utils (interruptOnSignal)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Mote (group)
import Test.DelegateServer.PlaceBid.Spec (suite) as PlaceBid

main :: Effect Unit
main = do
  fiber <- launchAff $ interpret suite
  interruptOnSignal SIGINT fiber *> interruptOnSignal SIGTERM fiber

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "delegate-server" do
    testPlutipContracts plutipConfig do
      PlaceBid.suite

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
      { slotLength: Seconds one
      , epochSize: Just $ UInt.fromInt 4320000
      , maxTxSize: Just $ UInt.fromInt 16384
      , raiseExUnitsToMax: true
      }
  }
