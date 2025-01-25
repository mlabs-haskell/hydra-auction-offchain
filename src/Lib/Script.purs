module HydraAuctionOffchain.Lib.Script
  ( decodeApplyScript
  ) where

import Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Types (PlutusData, PlutusScript)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Control.Monad.Error.Class (class MonadThrow, liftEither, liftMaybe)
import Data.Bifunctor (lmap)
import Effect.Exception (Error, error)

decodeApplyScript
  :: forall (m :: Type -> Type) (t :: Type)
   . MonadThrow Error m
  => { scriptEnvelope :: String
     , scriptName :: String
     , args :: Array PlutusData
     }
  -> m PlutusScript
decodeApplyScript { scriptEnvelope, scriptName, args } = do
  script <-
    liftMaybe (error $ "Could not decode " <> scriptName <> " envelope") do
      plutusScriptFromEnvelope
        =<< decodeTextEnvelope scriptEnvelope
  case args of
    [] -> pure script
    _ ->
      liftEither $
        lmap (error <<< append ("Could not apply args to " <> scriptName <> ": "))
          (applyArgs script args)
