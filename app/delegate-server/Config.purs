module DelegateServer.Config
  ( AppConfig
  , configParser
  ) where

import Prelude

import Data.Array (fromFoldable) as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Either (note)
import Data.Foldable (fold)
import DelegateServer.Types.HydraHeadPeer (HydraHeadPeer, hydraHeadPeerCodec)
import HydraAuctionOffchain.Config (HostPort, readHostPort)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import Node.Path (FilePath)
import Options.Applicative as Optparse
import Parsing (Parser, runParser)
import URI.Port (Port)
import URI.Port (parser) as Port

type AppConfig =
  { clientServerPort :: Port
  , hydraNodeId :: String
  , hydraNode :: HostPort
  , hydraNodeApi :: HostPort
  , hydraPersistDir :: FilePath
  , hydraSk :: FilePath
  , cardanoSk :: FilePath
  , peers :: Array HydraHeadPeer
  , nodeSocketPreprod :: FilePath
  }

configParser :: Optparse.Parser AppConfig
configParser = ado
  clientServerPort <- Optparse.option parsePort $ fold
    [ Optparse.long "client-server-port"
    , Optparse.metavar "PORT"
    ]
  hydraNodeId <- Optparse.strOption $ fold
    [ Optparse.long "hydra-node-id"
    , Optparse.metavar "STR"
    ]
  hydraNode <- Optparse.option parseHostPort $ fold
    [ Optparse.long "hydra-node"
    , Optparse.metavar "HOSTPORT"
    ]
  hydraNodeApi <- Optparse.option parseHostPort $ fold
    [ Optparse.long "hydra-node-api"
    , Optparse.metavar "HOSTPORT"
    ]
  hydraPersistDir <- Optparse.strOption $ fold
    [ Optparse.long "hydra-persist-dir"
    , Optparse.metavar "DIR"
    ]
  hydraSk <- Optparse.strOption $ fold
    [ Optparse.long "hydra-sk"
    , Optparse.metavar "FILE"
    ]
  cardanoSk <- Optparse.strOption $ fold
    [ Optparse.long "cardano-sk"
    , Optparse.metavar "FILE"
    ]
  peers <- Optparse.many $ Optparse.option parseHydraHeadPeer $ fold
    [ Optparse.long "peer"
    , Optparse.metavar "JSON"
    ]
  nodeSocketPreprod <- Optparse.strOption $ fold
    [ Optparse.long "node-socket-preprod"
    , Optparse.metavar "FILE"
    ]
  in
    { clientServerPort
    , hydraNodeId
    , hydraNode
    , hydraNodeApi
    , hydraPersistDir
    , hydraSk
    , cardanoSk
    , peers: Array.fromFoldable peers
    , nodeSocketPreprod
    }

parseHostPort :: Optparse.ReadM HostPort
parseHostPort =
  Optparse.eitherReader $ \str ->
    note ("Can't parse as HostPort: `" <> str <> "`") $ readHostPort str

parseHydraHeadPeer :: Optparse.ReadM HydraHeadPeer
parseHydraHeadPeer = parseJson "HydraHeadPeer" hydraHeadPeerCodec

parseJson :: forall (a :: Type). String -> CA.JsonCodec a -> Optparse.ReadM a
parseJson typeName codec =
  Optparse.eitherReader $ \str ->
    caDecodeString codec str # lmap \err ->
      "Can't parse as " <> typeName <> ": `" <> str <> "` ~ " <> err

parsePort :: Optparse.ReadM Port
parsePort = parserReader "Port" Port.parser

parserReader :: forall a. String -> Parser String a -> Optparse.ReadM a
parserReader typeName parser =
  Optparse.eitherReader $ \str ->
    runParser str parser # lmap \err ->
      "Can't parse as " <> typeName <> ": `" <> str <> "` ~ " <> show err
