module HydraAuctionOffchain.Contract.PersonalOracle
  ( PersonalOracle
  , mkPersonalOracle
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Hashing (nativeScriptHash)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Scripts
  ( MintingPolicy(NativeMintingPolicy)
  , NativeScript(ScriptPubkey)
  , ScriptHash
  )
import Contract.Value (mkTokenName, scriptCurrencySymbol)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import HydraAuctionOffchain.Contract.Types (AssetClass, mkAssetClass)
import Partial.Unsafe (unsafePartial)

type PersonalOracle =
  { assetClass :: AssetClass
  , nativeScript :: NativeScript
  , nativeScriptHash :: ScriptHash
  }

mkPersonalOracle :: PaymentPubKeyHash -> PersonalOracle
mkPersonalOracle pkh =
  { assetClass:
      mkAssetClass
        (unsafePartial fromJust $ scriptCurrencySymbol $ NativeMintingPolicy script)
        ( unsafePartial fromJust
            (mkTokenName =<< byteArrayFromAscii "PERSONAL_ORACLE")
        )
  , nativeScript: script
  , nativeScriptHash: unwrap $ nativeScriptHash script
  }
  where
  script :: NativeScript
  script = ScriptPubkey $ unwrap $ unwrap pkh
