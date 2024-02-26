module DelegateServer.Config
  ( AppConfig
  , configParser
  ) where

import Prelude

import Data.Either (note)
import Data.Foldable (fold)
import HydraAuctionOffchain.Config (HostPort, readHostPort)
import Node.Path (FilePath)
import Options.Applicative as Optparse

type AppConfig =
  { hydraNode :: HostPort
  , hydraNodeApi :: HostPort
  , hydraSk :: FilePath
  , cardanoSk :: FilePath
  , nodeSocketPreprod :: FilePath
  }

configParser :: Optparse.Parser AppConfig
configParser = ado
  hydraNode <- Optparse.option parseHostPort $ fold
    [ Optparse.long "hydra-node"
    , Optparse.metavar "HOSTPORT"
    ]
  hydraNodeApi <- Optparse.option parseHostPort $ fold
    [ Optparse.long "hydra-node-api"
    , Optparse.metavar "HOSTPORT"
    ]
  hydraSk <- Optparse.strOption $ fold
    [ Optparse.long "hydra-sk"
    , Optparse.metavar "FILE"
    ]
  cardanoSk <- Optparse.strOption $ fold
    [ Optparse.long "cardano-sk"
    , Optparse.metavar "FILE"
    ]
  nodeSocketPreprod <- Optparse.strOption $ fold
    [ Optparse.long "node-socket-preprod"
    , Optparse.metavar "FILE"
    ]
  in
    { hydraNode
    , hydraNodeApi
    , hydraSk
    , cardanoSk
    , nodeSocketPreprod
    }

parseHostPort :: Optparse.ReadM HostPort
parseHostPort =
  Optparse.eitherReader $ \str ->
    note ("Can't parse as HostPort: `" <> show str <> "`") $ readHostPort str
