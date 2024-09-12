module HydraAuctionOffchain.Types.ContractConfig
  ( ContractConfig(ContractConfig, ContractConfigPlutipEnv)
  , contractConfigCodec
  , localnetConfig
  , mkContractParams
  ) where

import Prelude

import Cardano.Types (NetworkId(MainnetId))
import Contract.Config
  ( ContractParams
  , LogLevel(Trace)
  , PrivatePaymentKeySource(PrivatePaymentKeyValue)
  , QueryBackendParams
  , WalletSpec(UseKeys)
  , defaultConfirmTxDelay
  , defaultTimeParams
  , emptyHooks
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  , strictSynchronizationParams
  )
import Contract.Test.Testnet (Era(Conway), TestnetConfig)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Data.Variant (inj, match) as Variant
import Effect.Aff (Aff)
import HydraAuctionOffchain.Lib.Codec (class HasJson, fixTaggedSumCodec)
import HydraAuctionOffchain.Service.PlutipEnv (queryPlutipEnvPrivateKey)
import HydraAuctionOffchain.Types.HostPort (HostPort, hostPortCodec)
import HydraAuctionOffchain.Types.Network
  ( Network
  , blockfrostPublicServerConfig
  , networkCodec
  , toCtlNetworkId
  )
import HydraAuctionOffchain.Types.WalletApp
  ( WalletApp
  , walletAppCodec
  , walletSpecFromWalletApp
  )
import Type.Proxy (Proxy(Proxy))

data ContractConfig
  = ContractConfig
      { network :: Network
      , blockfrostApiKey :: String
      , walletApp :: Maybe WalletApp
      }
  | ContractConfigPlutipEnv
      { demoHostPort :: HostPort
      , plutipEnvHostPort :: HostPort
      }

derive instance Generic ContractConfig _

instance Show ContractConfig where
  show = genericShow

instance HasJson ContractConfig anyParams where
  jsonCodec _ = const contractConfigCodec

contractConfigCodec :: CA.JsonCodec ContractConfig
contractConfigCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "network":
              Right $ CA.object "ContractConfig" $ CAR.record
                { network: networkCodec
                , blockfrostApiKey: CA.string
                , walletApp: CA.maybe walletAppCodec
                }
          , "plutip":
              Right $ CA.object "ContractConfigPlutipEnv" $ CAR.record
                { demoHostPort: hostPortCodec
                , plutipEnvHostPort: hostPortCodec
                }
          }
      )
  where
  toVariant = case _ of
    ContractConfig rec ->
      Variant.inj (Proxy :: _ "network") rec
    ContractConfigPlutipEnv rec ->
      Variant.inj (Proxy :: _ "plutip") rec

  fromVariant = Variant.match
    { "network": ContractConfig
    , "plutip": ContractConfigPlutipEnv
    }

mkContractParams :: ContractConfig -> Aff ContractParams
mkContractParams config =
  getWalletSpec <#> \walletSpec ->
    { backendParams
    , networkId
    , logLevel: Trace
    , walletSpec
    , customLogger: Nothing
    , suppressLogs: false
    , hooks: emptyHooks
    , timeParams: defaultTimeParams
    , synchronizationParams: strictSynchronizationParams
    }
  where
  backendParams :: QueryBackendParams
  backendParams = case config of
    ContractConfig rec ->
      mkBlockfrostBackendParams
        { blockfrostConfig: blockfrostPublicServerConfig rec.network
        , blockfrostApiKey: Just rec.blockfrostApiKey
        , confirmTxDelay: defaultConfirmTxDelay
        }
    ContractConfigPlutipEnv rec ->
      mkCtlBackendParams
        { ogmiosConfig: localnetConfig.ogmiosConfig
        , kupoConfig:
            { port: rec.demoHostPort.port
            , host: rec.demoHostPort.host
            , secure: false
            , path: Nothing
            }
        }

  networkId :: NetworkId
  networkId = case config of
    ContractConfig rec ->
      toCtlNetworkId rec.network
    ContractConfigPlutipEnv _ ->
      MainnetId

  getWalletSpec :: Aff (Maybe WalletSpec)
  getWalletSpec = case config of
    ContractConfig rec ->
      pure $ walletSpecFromWalletApp <$> rec.walletApp
    ContractConfigPlutipEnv rec ->
      Just <$> do
        let
          plutipEnvServerConfig =
            { port: rec.plutipEnvHostPort.port
            , host: rec.plutipEnvHostPort.host
            , secure: false
            , path: Nothing
            }
        payKey <- queryPlutipEnvPrivateKey plutipEnvServerConfig
        pure $ UseKeys (PrivatePaymentKeyValue payKey) Nothing Nothing

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
  , suppressLogs: false
  , customLogger: Nothing
  , hooks: emptyHooks
  , clusterConfig:
      { testnetMagic: 2
      , era: Conway
      , slotLength: Seconds 0.1
      , epochSize: Just $ UInt.fromInt 4320000
      }
  }
