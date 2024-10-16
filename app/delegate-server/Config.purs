module DelegateServer.Config
  ( AppConfig
  , AppConfig'(AppConfig)
  , AuctionSlotConfig
  , AuctionSlotRuntimeConfig
  , AuctionSlotConfigRec
  , DelegateServerConfig
  , deriveAuctionSlotRuntimeConfig
  , execAppConfigParser
  ) where

import Prelude

import Cardano.Types (Ed25519KeyHash, NetworkId(TestnetId, MainnetId), TransactionHash)
import Cardano.Types.PrivateKey (toPublicKey) as PrivateKey
import Cardano.Types.PublicKey (hash) as PublicKey
import Contract.Config (QueryBackendParams, defaultConfirmTxDelay)
import Contract.Transaction (TransactionInput)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile)
import Data.Codec.Argonaut (JsonCodec, array, int, number, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right), either)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Profunctor (dimap, wrapIso)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable (traverse)
import Data.Variant (inj, match) as Variant
import DelegateServer.Helpers (printOref, readOref)
import DelegateServer.Types.HydraHeadPeer (HydraHeadPeer, hydraHeadPeerCodec)
import DelegateServer.Types.QueryBackendParamsSimple
  ( QueryBackendParamsSimple
  , queryBackendParamsSimpleCodec
  , toQueryBackendParams
  )
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import HydraAuctionOffchain.Codec (logLevelCodec, portCodec, txHashCodec)
import HydraAuctionOffchain.Lib.Codec (fixTaggedSumCodec)
import HydraAuctionOffchain.Lib.Json (caDecodeFile)
import HydraSdk.Types (HostPort, Network, hostPortCodec, networkCodec)
import Node.Path (FilePath)
import Options.Applicative ((<**>))
import Options.Applicative as Optparse
import Record (merge) as Record
import Type.Proxy (Proxy(Proxy))
import URI.Port (Port)

-- TODO: check that specified ports are free and non-overlapping
-- TODO: check 'max number of participants' constraint
-- TODO: check that `auctionMetadataOref` exists and points to a valid auction
-- TODO: check the balance of `cardanoSk`
-- TODO: generate `hydraNodeId` using UUID
type AuctionSlotConfigRec (f :: Type -> Type) (ext :: Row Type) =
  ( auctionMetadataOref :: f TransactionInput
  -- ^ Reference of the tx output with auction metadata record,
  -- used to access onchain data of the target auction.
  , hydraNodeId :: String
  -- ^ The Hydra node identifier used on the Hydra network. It is
  -- important to have a unique identifier in order to be able
  -- to distinguish between connected peers.
  , hydraNode :: HostPort
  -- ^ Listen address + port for incoming Hydra network connections.
  , hydraNodeApi :: HostPort
  -- ^ Listen address + port for incoming client Hydra Node API
  -- connections.
  , cardanoSk :: FilePath
  -- ^ Cardano signing key of the underlying Hydra node, used to
  -- authorize Hydra protocol transactions, and any funds owned
  -- by this key will be used as 'fuel'.
  , peers :: Array HydraHeadPeer
  -- ^ Info about other Head participants (max 5 entries).
  | ext
  )

type AuctionSlotConfig (f :: Type -> Type) = Record (AuctionSlotConfigRec f ())

type AuctionSlotRuntimeConfig (f :: Type -> Type) =
  Record
    ( AuctionSlotConfigRec f
        ( delegatePkh :: Ed25519KeyHash
        )
    )

auctionConfigCodec :: CA.JsonCodec (AuctionSlotConfig Maybe)
auctionConfigCodec =
  CA.object "AuctionSlotConfig" $ CAR.record
    { auctionMetadataOref:
        CA.maybe $
          CA.prismaticCodec "TransactionInput" readOref printOref
            CA.string
    , hydraNodeId: CA.string
    , hydraNode: hostPortCodec
    , hydraNodeApi: hostPortCodec
    , peers: CA.array hydraHeadPeerCodec
    , cardanoSk: CA.string
    }

-- TODO: make `hydraScriptsTxHash` parameter optional
-- see https://github.com/input-output-hk/hydra/issues/1441
newtype AppConfig' (ac :: Type) (qb :: Type) = AppConfig
  { auctionConfig :: ac
  , serverPort :: Port
  -- ^ Listen port for incoming HTTP client requests to the
  -- delegate server.
  , wsServerPort :: Port
  -- ^ Listen port for incoming WebSocket connections to the
  -- delegate server.
  , hydraPersistDir :: FilePath
  -- ^ The directory where the state of underlying Hydra Heads will
  -- be stored.
  , hydraSk :: FilePath
  -- ^ Hydra signing key used by the underlying Hydra nodes.
  , nodeSocket :: FilePath
  -- ^ Filepath to local unix domain socket used to communicate
  -- with cardano-node.
  , network :: Network
  -- ^ Network identifier (mainnet or testnet+magic).
  , queryBackend :: qb
  -- ^ Cardano query backend (Blockfrost or Ogmios+Kupo) for CTL.
  , hydraScriptsTxHash :: TransactionHash
  -- ^ The transaction which is expected to have published Hydra
  -- scripts as reference scripts in its outputs. See hydra-node
  -- release notes for pre-published versions. You can use the
  -- 'publish-scripts' sub-command of hydra-node to publish them
  -- yourself.
  , hydraContestPeriod :: Int
  -- ^ Contestation period for close transaction in seconds. If this
  -- value is not in sync with other participants hydra-node will
  -- ignore the initial tx. Additionally, this value needs to make
  -- sense compared to the current network we are running.
  , slotReservationPeriod :: Seconds
  , logLevel :: LogLevel
  , ctlLogLevel :: LogLevel
  }

derive instance Newtype (AppConfig' ac qb) _

type AppConfig (f :: Type -> Type) =
  AppConfig' (AuctionSlotRuntimeConfig f) QueryBackendParams

type DelegateServerConfig =
  AppConfig' (Array (AuctionSlotRuntimeConfig Maybe)) QueryBackendParams

appConfigCodec
  :: CA.JsonCodec (AppConfig' (Array (AuctionSlotConfig Maybe)) QueryBackendParamsSimple)
appConfigCodec =
  wrapIso AppConfig $ CA.object "AppConfig" $ CAR.record
    { auctionConfig: CA.array auctionConfigCodec
    , serverPort: portCodec
    , wsServerPort: portCodec
    , hydraPersistDir: CA.string
    , hydraSk: CA.string
    , nodeSocket: CA.string
    , network: networkCodec
    , queryBackend: queryBackendParamsSimpleCodec
    , hydraScriptsTxHash: txHashCodec
    , hydraContestPeriod: CA.int
    , slotReservationPeriod: wrapIso Seconds CA.number
    , logLevel: logLevelCodec
    , ctlLogLevel: logLevelCodec
    }

deriveAuctionSlotRuntimeConfig
  :: forall (f :: Type -> Type)
   . AuctionSlotConfig f
  -> Aff (AuctionSlotRuntimeConfig f)
deriveAuctionSlotRuntimeConfig slotConfig = do
  cardanoSk <- unwrap <$> privatePaymentKeyFromFile slotConfig.cardanoSk
  let delegatePkh = PublicKey.hash $ PrivateKey.toPublicKey cardanoSk
  pure $ Record.merge slotConfig
    { delegatePkh
    }

execAppConfigParser :: Aff DelegateServerConfig
execAppConfigParser = do
  fp <- liftEffect $ Optparse.execParser parserInfo
  appConfig <- liftEffect $ either throw pure =<< caDecodeFile appConfigCodec fp
  auctionConfig <- traverse deriveAuctionSlotRuntimeConfig (unwrap appConfig).auctionConfig
  pure $ over wrap
    ( \rec -> rec
        { auctionConfig = auctionConfig
        , queryBackend =
            toQueryBackendParams rec.queryBackend defaultConfirmTxDelay
        }
    )
    appConfig
  where
  parserInfo :: Optparse.ParserInfo FilePath
  parserInfo =
    Optparse.info (configFileParser <**> Optparse.helper) $ Optparse.fullDesc
      <> Optparse.header "delegate-server"

  configFileParser :: Optparse.Parser FilePath
  configFileParser =
    Optparse.strOption $ fold
      [ Optparse.long "config"
      , Optparse.metavar "FILE"
      , Optparse.help "Filepath to delegate-server JSON configuration."
      ]
