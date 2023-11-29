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
import Data.BigInt (BigInt)
import HydraAuctionOffchain.Contract.MintingPolicies (mkAlwaysMintsPolicy)
import HydraAuctionOffchain.Contract.Types (emptySubmitTxData, submitTxReturningContractResult)

mintTokenUsingAlwaysMints :: TokenName -> BigInt -> Contract TransactionHash
mintTokenUsingAlwaysMints tokenName quantity = do
  alwaysMintsPolicy <- mkAlwaysMintsPolicy
  let
    alwaysMintsPolicyHash :: MintingPolicyHash
    alwaysMintsPolicyHash = mintingPolicyHash alwaysMintsPolicy

    constraints :: TxConstraints Void Void
    constraints = Constraints.mustMintCurrency alwaysMintsPolicyHash tokenName quantity

    lookups :: ScriptLookups Void
    lookups = Lookups.mintingPolicy alwaysMintsPolicy

  map _.txHash $ submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }
