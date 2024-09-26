module DelegateServer.Lib.Transaction
  ( appendTxSignatures
  , reSignTransaction
  , setAuxDataHash
  , setExUnitsToMax
  , setTxValid
  , txSignatures
  ) where

import Prelude

import Cardano.Types (Language(PlutusV2), Transaction, Vkeywitness)
import Cardano.Types.AuxiliaryData (hashAuxiliaryData)
import Cardano.Types.Transaction (_body, _isValid, _witnessSet)
import Cardano.Types.TransactionBody (_auxiliaryDataHash)
import Cardano.Types.TransactionWitnessSet (_plutusData, _redeemers, _vkeys)
import Contract.Monad (Contract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction (signTransaction)
import Ctl.Internal.Transaction (setScriptDataHash)
import Data.Lens (view, (%~), (.~), (<>~), (^.))
import Data.Map (filterKeys) as Map
import Data.Newtype (modify, unwrap)
import Effect.Class (liftEffect)

setAuxDataHash :: Transaction -> Transaction
setAuxDataHash tx =
  tx # _body <<< _auxiliaryDataHash .~
    (hashAuxiliaryData <$> (unwrap tx).auxiliaryData)

reSignTransaction :: Transaction -> Contract Transaction
reSignTransaction tx = signTransaction (tx # _witnessSet <<< _vkeys .~ mempty)

setTxValid :: Transaction -> Transaction
setTxValid = _isValid .~ true

setExUnitsToMax :: Transaction -> Contract Transaction
setExUnitsToMax tx = do
  pparams <- unwrap <$> getProtocolParameters
  let
    costModels = Map.filterKeys (eq PlutusV2) pparams.costModels
    ws = evaluatedTx ^. _witnessSet
    evaluatedTx =
      tx # _witnessSet <<< _redeemers %~ map \redeemer ->
        redeemer # modify _
          { exUnits = pparams.maxTxExUnits
          }
  liftEffect $
    setScriptDataHash costModels (ws ^. _redeemers) (ws ^. _plutusData)
      evaluatedTx

txSignatures :: Transaction -> Array Vkeywitness
txSignatures = view (_witnessSet <<< _vkeys)

appendTxSignatures :: Array Vkeywitness -> Transaction -> Transaction
appendTxSignatures signatures =
  _witnessSet <<< _vkeys <>~ signatures
