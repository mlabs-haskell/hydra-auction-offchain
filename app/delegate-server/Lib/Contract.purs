module DelegateServer.Lib.Contract
  ( runContractNullCostsAff
  ) where

import Prelude

import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Contract.Numeric.BigNum (one, zero) as BigNum
import Contract.ProtocolParameters (getProtocolParameters)
import Control.Monad.Reader (local)
import Data.Newtype (modify)
import Data.UInt (UInt)
import Effect.Aff (Aff)

runContractNullCostsAff :: forall (a :: Type). ContractEnv -> Contract a -> Aff a
runContractNullCostsAff contractEnv contract =
  runContractInEnv contractEnv do
    pparams <- getProtocolParameters <#> modify \rec ->
      rec
        { txFeeFixed = (zero :: UInt)
        , txFeePerByte = (zero :: UInt)
        , prices =
            { memPrice: { numerator: BigNum.zero, denominator: BigNum.one }
            , stepPrice: { numerator: BigNum.zero, denominator: BigNum.one }
            }
        }
    contract # local _
      { ledgerConstants =
          contractEnv.ledgerConstants
            { pparams = pparams
            }
      }
