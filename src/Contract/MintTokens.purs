module HydraAuctionOffchain.Contract.MintTokens
  ( mintTokenUsingAlwaysMints
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (mintingPolicy) as Lookups
import Contract.Scripts (MintingPolicyHash, mintingPolicyHash)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustMintCurrency) as Constraints
import Contract.Value (TokenName)
import HydraAuctionOffchain.Contract.MintingPolicies (mkAlwaysMintsPolicy)
import HydraAuctionOffchain.Contract.Types (emptySubmitTxData, submitTxReturningContractResult)
import JS.BigInt (BigInt)

mintTokenUsingAlwaysMints :: TokenName -> BigInt -> Contract TransactionHash
mintTokenUsingAlwaysMints tokenName quantity = do
  alwaysMintsPolicy <- mkAlwaysMintsPolicy
  let
    alwaysMintsPolicyHash :: MintingPolicyHash
    alwaysMintsPolicyHash = mintingPolicyHash alwaysMintsPolicy

    constraints :: TxConstraints
    constraints = Constraints.mustMintCurrency alwaysMintsPolicyHash tokenName quantity

    lookups :: ScriptLookups
    lookups = Lookups.mintingPolicy alwaysMintsPolicy

  map _.txHash $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }
