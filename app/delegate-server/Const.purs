module DelegateServer.Const
  ( AppConst
  , appConst
  ) where

import Cardano.Types (BigNum)
import Cardano.Types.BigNum (fromInt) as BigNum
import JS.BigInt (fromInt) as BigInt
import Node.Path (FilePath)

type AppConst =
  { testnetMagic :: String
  , protocolParams :: FilePath
  , collateralLovelace :: BigNum
  }

appConst :: AppConst
appConst =
  { testnetMagic: "1"
  , protocolParams: "protocol-parameters.json"
  , collateralLovelace: BigNum.fromInt 10_000_000
  }
