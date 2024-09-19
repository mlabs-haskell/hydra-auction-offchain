module Test.Main
  ( main
  ) where

import Prelude

import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Testnet (testTestnetContracts)
import Contract.Test.Utils (interruptOnSignal)
import Data.Maybe (Maybe(Just))
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Time.Duration (Minutes(Minutes), fromDuration)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Ref (new, write) as Ref
import Mote (group)
import Test.Contract.AnnounceAuction (suite) as AnnounceAuction
import Test.Contract.AuthorizeBidders (suite) as AuthorizeBidders
import Test.Contract.DelegateServer (suite) as DelegateServer
import Test.Contract.EnterAuction (suite) as EnterAuction
import Test.Contract.PlaceBid (suite) as PlaceBid
import Test.Contract.StartBidding (suite) as StartBidding
import Test.DelegateServer.WsServer (suite) as WsServer
import Test.Localnet.Config (localnetConfig)
import Test.Spec.Runner (Config, defaultConfig) as SpecRunner

main :: Effect Unit
main = do
  fiber <- launchAff $ interpretWithConfig runnerConfig suite
  interruptOnSignal SIGINT fiber *> interruptOnSignal SIGTERM fiber
  where
  runnerConfig :: SpecRunner.Config
  runnerConfig =
    SpecRunner.defaultConfig
      { timeout = Just (fromDuration $ Minutes 10.0)
      }

suite :: TestPlanM (Aff Unit) Unit
suite = do
  clusterParamsRef <- liftEffect $ Ref.new { nodeSocketPath: mempty }
  let
    config =
      localnetConfig
        { hooks = localnetConfig.hooks
            { onClusterStartup = Just (flip Ref.write clusterParamsRef)
            }
        }
  group "delegate-server" do
    WsServer.suite
    testTestnetContracts config do
      DelegateServer.suite clusterParamsRef

      group "contracts" do
        AnnounceAuction.suite
        StartBidding.suite
        EnterAuction.suite
        AuthorizeBidders.suite
        PlaceBid.suite
