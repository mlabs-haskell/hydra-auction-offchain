module DelegateServer.Lib.Transaction
  ( setExUnitsToMax
  , setTxValid
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction
  ( Language(PlutusV2)
  , Transaction
  , _isValid
  , _plutusData
  , _witnessSet
  )
import Ctl.Internal.Cardano.Types.Transaction (_redeemers)
import Ctl.Internal.Transaction (setScriptDataHash)
import Data.Lens ((^.), (%~), (.~))
import Data.Map (filterKeys) as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (modify, unwrap, wrap)
import Effect.Class (liftEffect)

setTxValid :: Transaction -> Transaction
setTxValid = _isValid .~ true

setExUnitsToMax :: Transaction -> Contract Transaction
setExUnitsToMax tx = do
  pparams <- unwrap <$> getProtocolParameters
  let
    costModels = modify (Map.filterKeys (eq PlutusV2)) pparams.costModels
    ws = evaluatedTx ^. _witnessSet
    evaluatedTx =
      tx # _witnessSet <<< _redeemers %~ map
        ( map \redeemer ->
            redeemer # modify _
              { exUnits = pparams.maxTxExUnits
              }
        )
  liftEffect $
    setScriptDataHash costModels (fromMaybe mempty $ ws ^. _redeemers)
      (wrap <$> fromMaybe mempty (ws ^. _plutusData))
      evaluatedTx
