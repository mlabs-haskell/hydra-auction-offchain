module HydraAuctionOffchain.Contract.Validators.DelegateGroupMetadata
  ( mkDelegateGroupMetadataValidator
  ) where

import Prelude

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import delegateGroupMetadataValidator :: String

mkDelegateGroupMetadataValidator :: Contract PlutusScript
mkDelegateGroupMetadataValidator =
  decodeApplyScript
    { scriptEnvelope: delegateGroupMetadataValidator
    , scriptName: "DelegateGroupMetadataValidator"
    , args: mempty
    }
