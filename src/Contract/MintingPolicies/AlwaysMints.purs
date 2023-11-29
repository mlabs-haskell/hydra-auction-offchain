module HydraAuctionOffchain.Contract.MintingPolicies.AlwaysMints
  ( mkAlwaysMintsPolicy
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Value (CurrencySymbol, scriptCurrencySymbol)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

foreign import alwaysMintsPolicy :: String

mkAlwaysMintsPolicy :: Contract MintingPolicy
mkAlwaysMintsPolicy =
  PlutusMintingPolicy <$>
    liftMaybe (error "Error decoding alwaysMintsPolicy") do
      envelope <- decodeTextEnvelope alwaysMintsPolicy
      plutusScriptV2FromEnvelope envelope
