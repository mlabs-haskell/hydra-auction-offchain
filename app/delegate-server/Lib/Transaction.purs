module DelegateServer.Lib.Transaction
  ( appendTxSignatures
  , reSignTransaction
  , setAuxDataHash
  , setExUnitsToMax
  , setTxValid
  , txSignatures
  ) where

import Prelude

import Contract.Hashing (auxiliaryDataHash)
import Contract.Monad (Contract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction
  ( BalancedSignedTransaction
  , Language(PlutusV2)
  , Transaction
  , Vkeywitness
  , _auxiliaryDataHash
  , _body
  , _isValid
  , _plutusData
  , _vkeys
  , _witnessSet
  , signTransaction
  )
import Ctl.Internal.Cardano.Types.Transaction (_redeemers)
import Ctl.Internal.Transaction (setScriptDataHash)
import Data.Lens (view, (%~), (.~), (^.))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (filterKeys) as Map
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Newtype (modify, unwrap, wrap)
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect, liftEffect)

setAuxDataHash :: forall m. MonadEffect m => Transaction -> m Transaction
setAuxDataHash tx = do
  auxDataHash <- liftEffect $ traverse auxiliaryDataHash (unwrap tx).auxiliaryData
  pure $ tx # _body <<< _auxiliaryDataHash .~ auxDataHash

reSignTransaction :: BalancedSignedTransaction -> Contract BalancedSignedTransaction
reSignTransaction tx =
  signTransaction
    (tx # simple _Newtype <<< _witnessSet <<< _vkeys .~ Nothing)

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

txSignatures :: Transaction -> Array Vkeywitness
txSignatures = fromMaybe mempty <<< view (_witnessSet <<< _vkeys)

appendTxSignatures :: Array Vkeywitness -> Transaction -> Transaction
appendTxSignatures signatures =
  _witnessSet <<< _vkeys %~
    pure <<< append signatures <<< fromMaybe mempty
