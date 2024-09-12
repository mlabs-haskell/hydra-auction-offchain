module HydraAuctionOffchain.Contract.MintTokens
  ( mintTokenUsingAlwaysMints
  ) where

import Contract.Prelude

import Cardano.Types (BigNum, ScriptHash)
import Cardano.Types.Int (newPositive) as Cardano.Int
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (plutusMintingPolicy) as Lookups
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustMintCurrency) as Constraints
import Contract.Value (TokenName)
import HydraAuctionOffchain.Contract.MintingPolicies (mkAlwaysMintsPolicy)
import HydraAuctionOffchain.Contract.Types (emptySubmitTxData, submitTxReturningContractResult)
import JS.BigInt (BigInt)

mintTokenUsingAlwaysMints :: TokenName -> BigNum -> Contract TransactionHash
mintTokenUsingAlwaysMints tokenName quantity = do
  alwaysMintsPolicy <- mkAlwaysMintsPolicy
  let
    alwaysMintsPolicyHash :: ScriptHash
    alwaysMintsPolicyHash = PlutusScript.hash alwaysMintsPolicy

    constraints :: TxConstraints
    constraints =
      Constraints.mustMintCurrency alwaysMintsPolicyHash tokenName $
        Cardano.Int.newPositive quantity

    lookups :: ScriptLookups
    lookups = Lookups.plutusMintingPolicy alwaysMintsPolicy

  map _.txHash $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }
