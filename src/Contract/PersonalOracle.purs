module HydraAuctionOffchain.Contract.PersonalOracle
  ( PersonalOracle
  , mkPersonalOracle
  ) where

import Contract.Prelude

import Cardano.Types (Address, NativeScript(ScriptPubkey), NetworkId, ScriptHash)
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.NativeScript (hash) as NativeScript
import Contract.Address (PaymentPubKeyHash)
import Contract.Hashing (nativeScriptHash)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Data.Newtype (unwrap)
import HydraAuctionOffchain.Contract.Types (AssetClass, mkAssetClass)
import HydraAuctionOffchain.Helpers (fromJustWithErr)
import HydraAuctionOffchain.Lib.Cardano.Address (scriptHashAddress)

type PersonalOracle =
  { assetClass :: AssetClass
  , nativeScript :: NativeScript
  , nativeScriptHash :: ScriptHash
  , address :: Address
  }

mkPersonalOracle :: NetworkId -> PaymentPubKeyHash -> PersonalOracle
mkPersonalOracle network pkh =
  { assetClass:
      mkAssetClass
        scriptHash
        ( fromJustWithErr "mkPersonalOracle"
            (mkAssetName =<< byteArrayFromAscii "PERSONAL_ORACLE")
        )
  , nativeScript: script
  , nativeScriptHash: scriptHash
  , address: scriptHashAddress network scriptHash
  }
  where
  script :: NativeScript
  script = ScriptPubkey $ unwrap pkh

  scriptHash :: ScriptHash
  scriptHash = NativeScript.hash script
