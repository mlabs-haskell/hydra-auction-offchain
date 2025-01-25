module DelegateServer.Lib.Contract
  ( runContractNullCostsAff
  ) where

import Prelude

import Cardano.Types.Coin (zero) as Coin
import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Contract.Numeric.BigNum (one, zero) as BigNum
import Contract.ProtocolParameters (getProtocolParameters)
import Control.Monad.Reader (local)
import Data.Newtype (modify, wrap)
import Data.UInt (UInt)
import Effect.Aff (Aff)

runContractNullCostsAff :: forall (a :: Type). ContractEnv -> Contract a -> Aff a
runContractNullCostsAff contractEnv contract =
  runContractInEnv contractEnv do
    pparams <- getProtocolParameters <#> modify \rec ->
      rec
        { txFeeFixed = Coin.zero
        , txFeePerByte = (zero :: UInt)
        , prices = wrap
            { memPrice: wrap { numerator: BigNum.zero, denominator: BigNum.one }
            , stepPrice: wrap { numerator: BigNum.zero, denominator: BigNum.one }
            }
        }
    contract # local _
      { ledgerConstants =
          contractEnv.ledgerConstants
            { pparams = pparams
            }
      }
