module HydraAuctionOffchain.Contract.MintingPolicies.DelegateGroup
  ( delegateGroupTokenName
  , mkDelegateGroupMintingPolicy
  ) where

import Cardano.ToData (toData)
import Cardano.Types (AssetName, PlutusScript, ScriptHash)
import Contract.Monad (Contract)
import Contract.Transaction (TransactionInput)
import HydraAuctionOffchain.Helpers (tokenNameFromAsciiUnsafe)
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import delegateGroupMintingPolicy :: String

mkDelegateGroupMintingPolicy :: ScriptHash -> TransactionInput -> Contract PlutusScript
mkDelegateGroupMintingPolicy delegateGroupMetadataSh nonceOref =
  decodeApplyScript
    { scriptEnvelope: delegateGroupMintingPolicy
    , scriptName: "DelegateGroupMintingPolicy"
    , args:
        [ toData delegateGroupMetadataSh
        , toData nonceOref
        ]
    }

-- | Delegate group metadata token, identifying the true delegate group.
delegateGroupTokenName :: AssetName
delegateGroupTokenName = tokenNameFromAsciiUnsafe "DELEGATE_GROUP"
