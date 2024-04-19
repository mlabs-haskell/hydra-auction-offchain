module PlutipEnv.Config
  ( PlutipEnvConfig
  , configParser
  ) where

import Prelude

import Data.Foldable (fold)
import HydraAuctionOffchain.Types.HostPort (HostPort, parseHostPort)
import Node.Path (FilePath)
import Options.Applicative as Optparse

type PlutipEnvConfig =
  { plutipEnvHostPort :: HostPort
  , demoHostPort :: HostPort
  , paymentSkeyFilePath :: FilePath
  }

configParser :: Optparse.Parser PlutipEnvConfig
configParser = ado
  plutipEnvHostPort <- Optparse.option parseHostPort $ fold
    [ Optparse.long "plutip-env"
    , Optparse.metavar "HOSTPORT"
    ]
  demoHostPort <- Optparse.option parseHostPort $ fold
    [ Optparse.long "demo"
    , Optparse.metavar "HOSTPORT"
    ]
  paymentSkeyFilePath <- Optparse.strOption $ fold
    [ Optparse.long "payment-skey-file"
    , Optparse.metavar "FILEPATH"
    , Optparse.help "File to store the generated KeyWallet payment signing key"
    ]
  in
    { plutipEnvHostPort
    , demoHostPort
    , paymentSkeyFilePath
    }
