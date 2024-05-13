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
  , hydraScriptsTxHash :: String
  , collateralLovelace :: BigInt
  }

appConst :: AppConst
appConst =
  { testnetMagic: "1"
  , protocolParams: "protocol-parameters.json"
  , hydraScriptsTxHash: "8ce483e2d4b81f9254392afda1f85d1e123165665593228c39064691903f431a"
  , collateralLovelace: BigInt.fromInt 10_000_000
  }
