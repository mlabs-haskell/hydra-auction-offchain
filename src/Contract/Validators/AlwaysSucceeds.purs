module HydraAuctionOffchain.Contract.Validators.AlwaysSucceeds
  ( mkAlwaysSucceedsValidator
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator(Validator))
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Control.Monad.Error.Class (liftMaybe)
import Effect.Exception (error)

foreign import alwaysSucceedsValidator :: String

mkAlwaysSucceedsValidator :: Contract Validator
mkAlwaysSucceedsValidator =
  Validator <$>
    liftMaybe (error "Error decoding alwaysSucceedsValidator") do
      envelope <- decodeTextEnvelope alwaysSucceedsValidator
      plutusScriptV2FromEnvelope envelope
