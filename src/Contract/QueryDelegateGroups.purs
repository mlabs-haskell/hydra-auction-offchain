module HydraAuctionOffchain.Contract.QueryDelegateGroups
  ( queryDelegateGroups
  ) where

import Contract.Prelude

import Cardano.Types (Asset(Asset), ScriptHash)
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.Value (valueOf) as Value
import Contract.Address (getNetworkId)
import Contract.Monad (Contract)
import Contract.Utxos (utxosAt)
import Control.Alternative (guard)
import Data.Array (mapMaybe) as Array
import Data.Map (toUnfoldable) as Map
import HydraAuctionOffchain.Contract.MintingPolicies.DelegateGroup (delegateGroupTokenName)
import HydraAuctionOffchain.Contract.Types (DelegateGroupInfo, Utxo)
import HydraAuctionOffchain.Contract.Validators (mkDelegateGroupMetadataValidator)
import HydraAuctionOffchain.Helpers (getInlineDatum)
import HydraAuctionOffchain.Lib.Cardano.Address (scriptHashAddress)

queryDelegateGroups :: Contract (Array DelegateGroupInfo)
queryDelegateGroups = do
  network <- getNetworkId
  delegateGroupMetadataAddr <- scriptHashAddress network <<< PlutusScript.hash <$>
    mkDelegateGroupMetadataValidator
  utxosAt delegateGroupMetadataAddr
    <#> Array.mapMaybe getValidDelegateGroup
    <<< Map.toUnfoldable

getValidDelegateGroup :: Utxo -> Maybe DelegateGroupInfo
getValidDelegateGroup (_ /\ txOut) = do
  delegateGroupInfo <- getInlineDatum txOut
  guard $ validDelegateGroupId (unwrap delegateGroupInfo).delegateGroupId
  pure delegateGroupInfo
  where
  validDelegateGroupId :: ScriptHash -> Boolean
  validDelegateGroupId cs =
    Value.valueOf (Asset cs delegateGroupTokenName) (unwrap txOut).amount
      == BigNum.one
