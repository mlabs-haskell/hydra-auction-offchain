module HydraAuctionOffchain.Config
  ( HostPort
  , config
  , demoServerConfig
  , hostPortCodec
  , mkContractParams
  , plutipConfig
  , printHostPort
  , readHostPort
  ) where

import Prelude

import Contract.Config
  ( ContractParams
  , LogLevel(Trace)
  , NetworkId(MainnetId, TestnetId)
  , ServerConfig
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreviewServerConfig
  , defaultConfirmTxDelay
  , defaultTimeParams
  , emptyHooks
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  , strictSynchronizationParams
  )
import Contract.Test.Plutip (PlutipConfig)
import Control.Error.Util (bool)
import Data.Codec.Argonaut (JsonCodec, prismaticCodec, string) as CA
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.String (Pattern(Pattern))
import Data.String (split) as String
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable (traverse)
import Data.UInt (UInt)
import Data.UInt (fromInt, fromString, toString) as UInt
import Effect.Aff (Aff)
import HydraAuctionOffchain.WalletApp (WalletApp(Plutip), walletSpecFromWalletApp)
import Partial.Unsafe (unsafePartial)

foreign import _isMainnet :: Boolean
foreign import _blockfrostApiKey :: String
foreign import _plutipEnvHostPort :: String
foreign import _demoHostPort :: String

type Config =
  { network :: NetworkId
  , blockfrostApiKey :: String
  , demoHostPort :: HostPort
  , plutipEnvHostPort :: HostPort
  }

type HostPort = { host :: String, port :: UInt }

hostPortCodec :: CA.JsonCodec HostPort
hostPortCodec = CA.prismaticCodec "HostPort" readHostPort printHostPort CA.string

printHostPort :: HostPort -> String
printHostPort { host, port } = host <> ":" <> UInt.toString port

readHostPort :: String -> Maybe HostPort
readHostPort str =
  case String.split (Pattern ":") str of
    [ host, port ] -> { host, port: _ } <$> UInt.fromString port
    _ -> Nothing

config :: Config
config =
  { network: bool TestnetId MainnetId _isMainnet
  , blockfrostApiKey: _blockfrostApiKey
  , plutipEnvHostPort: unsafePartial fromJust $ readHostPort _plutipEnvHostPort
  , demoHostPort: unsafePartial fromJust $ readHostPort _demoHostPort
  }

demoServerConfig :: ServerConfig
demoServerConfig =
  { port: config.demoHostPort.port
  , host: config.demoHostPort.host
  , secure: false
  , path: Nothing
  }

plutipEnvServerConfig :: ServerConfig
plutipEnvServerConfig =
  { port: config.plutipEnvHostPort.port
  , host: config.plutipEnvHostPort.host
  , secure: false
  , path: Nothing
  }

blockfrostServerConfig :: ServerConfig
blockfrostServerConfig =
  case config.network of
    TestnetId ->
      blockfrostPublicPreviewServerConfig
    MainnetId ->
      blockfrostPublicMainnetServerConfig

mkContractParams :: Maybe WalletApp -> Aff ContractParams
mkContractParams walletApp =
  traverse (walletSpecFromWalletApp plutipEnvServerConfig) walletApp <#> \walletSpec ->
    { backendParams:
        case walletApp of
          Just Plutip ->
            mkCtlBackendParams
              { ogmiosConfig: plutipConfig.ogmiosConfig
              , kupoConfig: demoServerConfig
              }
          _ ->
            mkBlockfrostBackendParams
              { blockfrostConfig: blockfrostServerConfig
              , blockfrostApiKey: Just config.blockfrostApiKey
              , confirmTxDelay: defaultConfirmTxDelay
              }
    , networkId: bool config.network MainnetId $ walletApp == Just Plutip
    , logLevel: Trace
    , walletSpec
    , customLogger: Nothing
    , suppressLogs: false
    , hooks: emptyHooks
    , timeParams: defaultTimeParams
    , synchronizationParams: strictSynchronizationParams
    }

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
  , suppressLogs: false
  , customLogger: Nothing
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.1
      , epochSize: Just $ UInt.fromInt 4320000
      , maxTxSize: Just $ UInt.fromInt 16384
      , raiseExUnitsToMax: false
      }
  }
