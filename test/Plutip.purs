module Test.Plutip
  ( main
  ) where

import Prelude

import Contract.Test.Mote (TestPlanM, interpret)
import Contract.Test.Plutip (testPlutipContracts)
import Contract.Test.Utils (interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Mote (group, skip)
import Test.Contract.AnnounceAuction (suite) as AnnounceAuction
import Test.Contract.AuthorizeBidders (suite) as AuthorizeBidders
import Test.Contract.DelegateServer (suite) as DelegateServer
import Test.Contract.EnterAuction (suite) as EnterAuction
import Test.Contract.PlaceBid (suite) as PlaceBid
import Test.Contract.StartBidding (suite) as StartBidding
import Test.Plutip.Config (plutipConfig)

main :: Effect Unit
main = do
  fiber <- launchAff $ interpret suite
  interruptOnSignal SIGINT fiber *> interruptOnSignal SIGTERM fiber

suite :: TestPlanM (Aff Unit) Unit
suite =
  testPlutipContracts plutipConfig do
    group "contracts" do
      AnnounceAuction.suite
      StartBidding.suite
      EnterAuction.suite
      AuthorizeBidders.suite
      PlaceBid.suite
      skip DelegateServer.suite
