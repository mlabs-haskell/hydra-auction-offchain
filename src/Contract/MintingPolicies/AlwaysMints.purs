module HydraAuctionOffchain.Contract.MintingPolicies.AlwaysMints
  ( mkAlwaysMintsPolicy
  ) where

import Prelude

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import alwaysMintsPolicy :: String

mkAlwaysMintsPolicy :: Contract PlutusScript
mkAlwaysMintsPolicy =
  decodeApplyScript
    { scriptEnvelope: alwaysMintsPolicy
    , scriptName: "AlwaysMintsPolicy"
    , args: mempty
    }
