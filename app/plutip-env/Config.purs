module PlutipEnv.Config
  ( PlutipEnvConfig
  , configParser
  ) where

import Prelude

import Data.Foldable (fold)
import Node.Path (FilePath)
import Options.Applicative as Optparse

type PlutipEnvConfig =
  { paymentSkeyFilePath :: FilePath
  }

configParser :: Optparse.Parser PlutipEnvConfig
configParser =
  { paymentSkeyFilePath: _ } <$>
    ( Optparse.strOption $ fold
        [ Optparse.long "payment-skey-file"
        , Optparse.metavar "FILEPATH"
        , Optparse.help "File to store the generated KeyWallet payment signing key"
        ]
    )
