module Test.Plutip
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
import Test.Contract.AnnounceAuction (suite) as AnnounceAuction
import Test.Contract.AuthorizeBidders (suite) as AuthorizeBidders
import Test.Contract.EnterAuction (suite) as EnterAuction
import Test.Contract.PlaceBid (suite) as PlaceBid
import Test.Contract.StartBidding (suite) as StartBidding
-- import Test.DelegateServer.PlaceBid.Suite (suite) as PlaceBidL2

main :: Effect Unit
main = do
  fiber <- launchAff $ interpret suite
  interruptOnSignal SIGINT fiber *> interruptOnSignal SIGTERM fiber

suite :: TestPlanM (Aff Unit) Unit
suite =
  group "contracts" do
    testPlutipContracts plutipConfig do
      AnnounceAuction.suite
      StartBidding.suite
      EnterAuction.suite
      AuthorizeBidders.suite
      PlaceBid.suite

-- PlaceBidL2.suite

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
  , customLogger: Nothing
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.1
      , epochSize: Just $ UInt.fromInt 4320000
      , maxTxSize: Just $ UInt.fromInt 16384
      , raiseExUnitsToMax: true
      }
  }
