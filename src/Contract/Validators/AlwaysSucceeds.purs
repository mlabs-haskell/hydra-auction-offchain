module HydraAuctionOffchain.Contract.Validators.AlwaysSucceeds
  ( mkAlwaysSucceedsValidator
  ) where

import Contract.Prelude

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import alwaysSucceedsValidator :: String

mkAlwaysSucceedsValidator :: Contract PlutusScript
mkAlwaysSucceedsValidator =
  decodeApplyScript
    { scriptEnvelope: alwaysSucceedsValidator
    , scriptName: "AlwaysSucceedsValidator"
    , args: mempty
    }
