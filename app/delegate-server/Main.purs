module DelegateServer.Main where

import Prelude

import Ansi.Output (dim, withGraphics)
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.UInt (toString) as UInt
import Data.UUID (genUUID)
import Data.UUID (toString) as UUID
import DelegateServer.HydraNodeApi.WebSocket (initHydraNodeApiWsConn)
import DelegateServer.Config (AppConfig, configParser)
import DelegateServer.Const (appConst)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.ChildProcess (defaultSpawnOptions, spawn, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.Stream (onDataString)
import Options.Applicative ((<**>))
import Options.Applicative as Optparse

main :: Effect Unit
main = launchAff_ do
  appConfig <- liftEffect $ Optparse.execParser opts
  liftEffect $ startHydraNode appConfig

opts :: Optparse.ParserInfo AppConfig
opts =
  Optparse.info (configParser <**> Optparse.helper) $ Optparse.fullDesc
    <> Optparse.header "delegate-server"

startHydraNode :: AppConfig -> Effect Unit
startHydraNode appConfig = do
  args <- hydraNodeArgs
  hydraNodeProcess <- spawn "hydra-node" args defaultSpawnOptions
  onDataString (stdout hydraNodeProcess) Encoding.UTF8 \str -> do
    log $ withGraphics dim $ "[hydra-node] " <> str
  -- when (String.contains (Pattern "APIServerStarted") str) $
  --   initHydraNodeApiWsConn appConfig.hydraNodeApi
  where
  hydraNodeArgs :: Effect (Array String)
  hydraNodeArgs = do
    nodeId <- UUID.toString <$> genUUID
    pure
      [ "--node-id"
      , nodeId
      , "--host"
      , appConfig.hydraNode.host
      , "--port"
      , UInt.toString appConfig.hydraNode.port
      , "--api-host"
      , appConfig.hydraNodeApi.host
      , "--api-port"
      , UInt.toString appConfig.hydraNodeApi.port
      , "--hydra-signing-key"
      , appConfig.hydraSk
      , "--cardano-signing-key"
      , appConfig.cardanoSk
      , "--node-socket"
      , appConfig.nodeSocketPreprod
      , "--testnet-magic"
      , appConst.testnetMagic
      , "--ledger-protocol-parameters"
      , appConst.protocolParams
      , "--hydra-scripts-tx-id"
      , appConst.hydraScriptsTxHash
      ]
