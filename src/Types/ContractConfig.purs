module HydraAuctionOffchain.Types.ContractConfig
  ( ContractConfig(ContractConfig)
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
  , defaultKupoServerConfig
  , defaultOgmiosWsConfig
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
import Data.Newtype (class Newtype)
import Data.Profunctor (dimap, wrapIso)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Data.Variant (inj, match) as Variant
import Effect.Aff (Aff)
import HydraAuctionOffchain.Lib.Codec (class HasJson, fixTaggedSumCodec)
import HydraAuctionOffchain.Service.PlutipEnv (queryPlutipEnvPrivateKey)
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
import HydraSdk.Types (HostPort, hostPortCodec)
import Type.Proxy (Proxy(Proxy))

newtype ContractConfig = ContractConfig
  { network :: Network
  , blockfrostApiKey :: String
  , walletApp :: Maybe WalletApp
  }

derive instance Generic ContractConfig _
derive instance Newtype ContractConfig _

instance Show ContractConfig where
  show = genericShow

instance HasJson ContractConfig anyParams where
  jsonCodec _ = const contractConfigCodec

contractConfigCodec :: CA.JsonCodec ContractConfig
contractConfigCodec =
  wrapIso ContractConfig $ CA.object "ContractConfig" $ CAR.record
    { network: networkCodec
    , blockfrostApiKey: CA.string
    , walletApp: CA.maybe walletAppCodec
    }

mkContractParams :: ContractConfig -> ContractParams
mkContractParams (ContractConfig config) =
  { backendParams:
      mkBlockfrostBackendParams
        { blockfrostConfig: blockfrostPublicServerConfig config.network
        , blockfrostApiKey: Just config.blockfrostApiKey
        , confirmTxDelay: defaultConfirmTxDelay
        }
  , networkId: toCtlNetworkId config.network
  , logLevel: Trace
  , walletSpec: walletSpecFromWalletApp <$> config.walletApp
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: strictSynchronizationParams
  }

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
