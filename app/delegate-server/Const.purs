module DelegateServer.Const
  ( AppConst
  , appConst
  ) where

import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Node.Path (FilePath)

type AppConst =
  { testnetMagic :: String
  , protocolParams :: FilePath
  , hydraScriptsTxHash :: String
  , collateralLovelace :: BigInt
  }

appConst :: AppConst
appConst =
  { testnetMagic: "1"
  , protocolParams: "protocol-parameters.json"
  , hydraScriptsTxHash: "d8ba8c488f52228b200df48fe28305bc311d0507da2c2420b10835bf00d21948"
  , collateralLovelace: BigInt.fromInt 5_000_000
  }
