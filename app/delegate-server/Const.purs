module DelegateServer.Const
  ( AppConst
  , appConst
  ) where

import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import Node.Path (FilePath)

type AppConst =
  { testnetMagic :: String
  , protocolParams :: FilePath
  , collateralLovelace :: BigInt
  }

appConst :: AppConst
appConst =
  { testnetMagic: "1"
  , protocolParams: "protocol-parameters.json"
  , collateralLovelace: BigInt.fromInt 10_000_000
  }
