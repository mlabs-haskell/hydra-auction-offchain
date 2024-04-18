module DelegateServer.Config
  ( AppConfig
  , AppConfig'(AppConfig)
  , Network(Testnet, Mainnet)
  , Options
  , execAppConfigParser 
  , optionsParser
  ) where

import Prelude

import Contract.Config (QueryBackendParams(CtlBackendParams, BlockfrostBackendParams), ServerConfig, defaultConfirmTxDelay)
import Contract.Transaction (TransactionInput)
import Control.Alt ((<|>))
import Data.Array (fromFoldable) as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, array, int, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right), either, note)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel(Info))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Profunctor (dimap, wrapIso)
import Data.Show.Generic (genericShow)
import Data.String (null) as String
import Data.UInt (fromInt) as UInt
import Data.Variant (inj, match) as Variant
import DelegateServer.Helpers (printOref, readOref)
import DelegateServer.Lib.Codec (fixTaggedSumCodec)
import DelegateServer.Types.HydraHeadPeer (HydraHeadPeer, hydraHeadPeerCodec)
import DelegateServer.Types.QueryBackendParamsSimple (QueryBackendParamsSimple, queryBackendParamsSimpleCodec, toQueryBackendParams)
import Effect (Effect)
import Effect.Exception (throw)
import HydraAuctionOffchain.Codec (logLevelCodec, portCodec)
import HydraAuctionOffchain.Config (HostPort, hostPortCodec, readHostPort)
import HydraAuctionOffchain.Lib.Json (caDecodeFile, caDecodeString)
import Node.Path (FilePath)
import Options.Applicative as Optparse
import Options.Applicative.Types (optional) as Optparse
import Parsing (Parser, runParser)
import Parsing.String (rest, string)
import Type.Proxy (Proxy(Proxy))
import URI.Host (parser, print) as Host
import URI.Port (Port)
import URI.Port (parser, toInt) as Port

type AppConfig = AppConfig' QueryBackendParams

newtype AppConfig' (backend :: Type) = AppConfig
  { auctionMetadataOref :: TransactionInput
  , serverPort :: Port
  , wsServerPort :: Port
  , hydraNodeId :: String
  , hydraNode :: HostPort
  , hydraNodeApi :: HostPort
  , hydraPersistDir :: FilePath
  , hydraSk :: FilePath
  , cardanoSk :: FilePath
  , walletSk :: FilePath
  , peers :: Array HydraHeadPeer
  , nodeSocket :: FilePath
  , network :: Network
  , queryBackend :: backend
  , hydraScriptsTxHash :: String
  , hydraContestPeriod :: Int
  , logLevel :: LogLevel
  }

derive instance Newtype (AppConfig' a) _
derive instance Functor AppConfig'

appConfigCodec :: CA.JsonCodec (AppConfig' QueryBackendParamsSimple)
appConfigCodec =
  wrapIso AppConfig $ CA.object "AppConfig" $ CAR.record
    { auctionMetadataOref:
        CA.prismaticCodec "TransactionInput" readOref printOref
          CA.string
    , serverPort: portCodec
    , wsServerPort: portCodec
    , hydraNodeId: CA.string
    , hydraNode: hostPortCodec
    , hydraNodeApi: hostPortCodec
    , hydraPersistDir: CA.string
    , hydraSk: CA.string
    , cardanoSk: CA.string
    , walletSk: CA.string
    , peers: CA.array hydraHeadPeerCodec
    , nodeSocket: CA.string
    , network: networkCodec
    , queryBackend: queryBackendParamsSimpleCodec
    , hydraScriptsTxHash: CA.string
    , hydraContestPeriod: CA.int
    , logLevel: logLevelCodec
    }

execAppConfigParser :: Optparse.ParserInfo Options -> Effect AppConfig 
execAppConfigParser parserInfo = do
  opts <- Optparse.execParser parserInfo
  case opts of
    OptionsConfig appConfig ->
      pure appConfig
    OptionsConfigFile fp -> do
      appConfig <- either throw pure =<< caDecodeFile appConfigCodec fp
      pure $ appConfig <#>
        flip toQueryBackendParams defaultConfirmTxDelay

data Options
  = OptionsConfig AppConfig
  | OptionsConfigFile FilePath

optionsParser :: Optparse.Parser Options
optionsParser = (OptionsConfigFile <$> configFileParser)
  <|> (OptionsConfig <$> configParser)

configFileParser :: Optparse.Parser FilePath
configFileParser =
  Optparse.strOption $ fold
    [ Optparse.long "config"
    , Optparse.metavar "FILE"
    , Optparse.help "Filepath to delegate-server JSON configuration."
    ]

configParser :: Optparse.Parser AppConfig
configParser = ado
  auctionMetadataOref <- Optparse.option parseOref $ fold
    [ Optparse.long "auction-metadata-oref"
    , Optparse.metavar "TXOUTREF"
    , Optparse.help
        "Reference of the tx output with auction metadata record. \
        \This will be used to access onchain data for the target \
        \auction."
    ]
  serverPort <- Optparse.option parsePort $ fold
    [ Optparse.long "server-port"
    , Optparse.metavar "PORT"
    , Optparse.help
        "Listen port for incoming client connections to this \
        \delegate server."
    ]
  wsServerPort <- Optparse.option parsePort $ fold
    [ Optparse.long "ws-server-port"
    , Optparse.metavar "PORT"
    , Optparse.help
        "Listen port for incoming WebSocket connections to this \
        \delegate server."
    ]
  hydraNodeId <- Optparse.strOption $ fold
    [ Optparse.long "hydra-node-id"
    , Optparse.metavar "STR"
    , Optparse.help
        "The Hydra node identifier used on the Hydra network. It is \
        \important to have a unique identifier in order to be able \
        \to distinguish between connected peers."
    ]
  hydraNode <- Optparse.option parseHostPort $ fold
    [ Optparse.long "hydra-node"
    , Optparse.metavar "HOSTPORT"
    , Optparse.help
        "Listen address + port for incoming Hydra network \
        \connections."
    ]
  hydraNodeApi <- Optparse.option parseHostPort $ fold
    [ Optparse.long "hydra-node-api"
    , Optparse.metavar "HOSTPORT"
    , Optparse.help
        "Listen address + port for incoming client Hydra Node API \
        \connections."
    ]
  hydraPersistDir <- Optparse.strOption $ fold
    [ Optparse.long "hydra-persist-dir"
    , Optparse.metavar "DIR"
    , Optparse.help
        "The directory where the Hydra Head state is stored. Do not \
        \edit these files manually!"
    ]
  hydraSk <- Optparse.strOption $ fold
    [ Optparse.long "hydra-sk"
    , Optparse.metavar "FILE"
    , Optparse.help
        "Hydra signing key used by the underlying hydra-node."
    ]
  cardanoSk <- Optparse.strOption $ fold
    [ Optparse.long "cardano-sk"
    , Optparse.metavar "FILE"
    , Optparse.help
        "Cardano signing key of the underlying hydra-node. This will \
        \be used to authorize Hydra protocol transactions and any \
        \funds owned by this key will be used as 'fuel'."
    ]
  walletSk <- Optparse.strOption $ fold
    [ Optparse.long "wallet-sk"
    , Optparse.metavar "FILE"
    , Optparse.help
        "Cardano signing key of the wallet used to commit collateral \
        \to the hydra-node. It is necessary because hydra-node \
        \prohibits committing utxos controlled by 'cardanoSk'."
    ]
  peers <- Optparse.many $ Optparse.option parseHydraHeadPeer $ fold
    [ Optparse.long "peer"
    , Optparse.metavar "JSON"
    , Optparse.help
        "Peer data in JSON format: { \"hydraNode\": <HOSTPORT>, \
        \\"hydraVk\": <FILE>, \"cardanoVk\": <FILE> }."
    ]
  nodeSocket <- Optparse.strOption $ fold
    [ Optparse.long "node-socket"
    , Optparse.metavar "FILE"
    , Optparse.help
        "Filepath to local unix domain socket used to communicate \
        \with the cardano node."
    ]
  network <- networkParser
  queryBackend <- queryBackendParser
  hydraScriptsTxHash <- Optparse.strOption $ fold
    [ Optparse.long "hydra-scripts-tx-id"
    , Optparse.metavar "TXID"
    , Optparse.help
        "The transaction which is expected to have published Hydra \
        \scripts as reference scripts in its outputs. See hydra-node \
        \release notes for pre-published versions. You can use the \
        \'publish-scripts' sub-command of hydra-node to publish them \
        \yourself."
    ]
  hydraContestPeriod <- Optparse.option Optparse.int $ fold
    [ Optparse.long "hydra-contestation-period"
    , Optparse.metavar "SECONDS"
    , Optparse.showDefault
    , Optparse.value 60
    , Optparse.help
        "Contestation period for close transaction in seconds. If \
        \this value is not in sync with other participants \
        \hydra-node will ignore the initial tx. Additionally, this \
        \value needs to make sense compared to the current network \
        \we are running."
    ]
  logLevel <- Optparse.option parseLogLevel $ fold
    [ Optparse.long "log-level"
    , Optparse.metavar "LOGLEVEL"
    , Optparse.value Info
    ]
  in
    wrap
      { auctionMetadataOref
      , serverPort
      , wsServerPort
      , hydraNodeId
      , hydraNode
      , hydraNodeApi
      , hydraPersistDir
      , hydraSk
      , cardanoSk
      , walletSk
      , peers: Array.fromFoldable peers
      , nodeSocket
      , network
      , queryBackend
      , hydraScriptsTxHash
      , hydraContestPeriod
      , logLevel
      }

queryBackendParser :: Optparse.Parser QueryBackendParams
queryBackendParser = blockfrostBackendParser <|> ctlBackendParser

blockfrostBackendParser :: Optparse.Parser QueryBackendParams
blockfrostBackendParser = ado
  blockfrostConfig <- Optparse.option parseServerConfig $ fold
    [ Optparse.long "blockfrost-config"
    , Optparse.metavar "URL"
    , Optparse.help
        "Blockfrost server configuration. See \
        \ https://blockfrost.dev/api/blockfrost-io-api-documentation \
        \ for available public servers."
    ]
  blockfrostApiKey <- Optparse.optional $ Optparse.strOption $ fold
    [ Optparse.long "blockfrost-api-key"
    , Optparse.metavar "STR"
    , Optparse.help "Optional Blockfrost API key."
    ]
  in
    BlockfrostBackendParams
      { blockfrostConfig
      , blockfrostApiKey
      , confirmTxDelay: defaultConfirmTxDelay
      }
      Nothing

ctlBackendParser :: Optparse.Parser QueryBackendParams
ctlBackendParser = ado
  ogmiosConfig <- Optparse.option parseServerConfig $ fold
    [ Optparse.long "ogmios-config"
    , Optparse.metavar "URL"
    , Optparse.help "Ogmios server configuration."
    ]
  kupoConfig <- Optparse.option parseServerConfig $ fold
    [ Optparse.long "kupo-config"
    , Optparse.metavar "URL"
    , Optparse.help "Kupo server configuration."
    ]
  in
    CtlBackendParams { ogmiosConfig, kupoConfig }
      Nothing

data Network = Testnet { magic :: Int } | Mainnet

derive instance Generic Network _

instance Show Network where
  show = genericShow

networkCodec :: CA.JsonCodec Network
networkCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "testnet":
              Right $ CA.object "Testnet" $ CAR.record
                { magic: CA.int
                }
          , "mainnet": Left unit
          }
      )
  where
  toVariant = case _ of
    Testnet rec -> Variant.inj (Proxy :: _ "testnet") rec
    Mainnet -> Variant.inj (Proxy :: _ "mainnet") unit

  fromVariant = Variant.match
    { "testnet": Testnet
    , "mainnet": const Mainnet
    }

networkParser :: Optparse.Parser Network
networkParser = mainnetParser <|> testnetParser

mainnetParser :: Optparse.Parser Network
mainnetParser = Optparse.flag' Mainnet $ fold
  [ Optparse.long "mainnet"
  , Optparse.help "Use the mainnet magic id."
  ]

testnetParser :: Optparse.Parser Network
testnetParser =
  map (Testnet <<< { magic: _ }) $ Optparse.option Optparse.int $ fold
    [ Optparse.long "testnet-magic"
    , Optparse.metavar "INT"
    , Optparse.help
        "Network identifier for a testnet to connect to. We only \
        \need to provide the magic number here. For example: '2' is \
        \the 'preview' network. See \
        \https://book.world.dev.cardano.org/environments.html for \
        \available networks."
    ]

----------------------------------------------------------------------
-- Readers

parseOref :: Optparse.ReadM TransactionInput
parseOref =
  Optparse.eitherReader $ \str ->
    note ("Can't parse as TransactionInput: `" <> str <> "`") $ readOref str

parseHostPort :: Optparse.ReadM HostPort
parseHostPort =
  Optparse.eitherReader $ \str ->
    note ("Can't parse as HostPort: `" <> str <> "`") $ readHostPort str

parserReader :: forall a. String -> Parser String a -> Optparse.ReadM a
parserReader typeName parser =
  Optparse.eitherReader $ \str ->
    runParser str parser # lmap \err ->
      "Can't parse as " <> typeName <> ": `" <> str <> "` ~ " <> show err

parsePort :: Optparse.ReadM Port
parsePort = parserReader "Port" Port.parser

parseJson :: forall (a :: Type). String -> CA.JsonCodec a -> Optparse.ReadM a
parseJson typeName codec =
  Optparse.eitherReader $ \str ->
    caDecodeString codec str # lmap \err ->
      "Can't parse as " <> typeName <> ": `" <> str <> "` ~ " <> err

parseHydraHeadPeer :: Optparse.ReadM HydraHeadPeer
parseHydraHeadPeer = parseJson "HydraHeadPeer" hydraHeadPeerCodec

parseLogLevel :: Optparse.ReadM LogLevel
parseLogLevel = parseJson "LogLevel" logLevelCodec

parseServerConfig :: Optparse.ReadM ServerConfig
parseServerConfig = parserReader "ServerConfig" httpServerConfigParser

httpServerConfigParser :: Parser String ServerConfig
httpServerConfigParser = do
  secure <- (string "https://" $> true) <|> (string "http://" $> false)
  host <- Host.parser <#> Host.print
  port <- Port.parser <#> UInt.fromInt <<< Port.toInt
  path <- rest <#> \x -> if String.null x then Nothing else Just x
  pure { port, host, secure, path }
