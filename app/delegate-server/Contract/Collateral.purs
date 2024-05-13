module DelegateServer.Contract.Collateral
  ( getCollateralUtxo
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (OutputDatum(NoOutputDatum))
import Contract.Transaction (TransactionOutput(TransactionOutput), awaitTxConfirmed)
import Contract.TxConstraints (mustPayToPubKey) as Constraints
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet (getWalletUtxos, ownPaymentPubKeyHash)
import Data.Array (find) as Array
import Data.Map (toUnfoldable) as Map
import DelegateServer.Const (appConst)
import HydraAuctionOffchain.Contract.Types
  ( Utxo
  , emptySubmitTxData
  , submitTxReturningContractResult
  )

getCollateralUtxo :: Contract Utxo
getCollateralUtxo =
  queryCollateralUtxo >>=
    maybe
      ( createCollateralUtxo
          *> liftedM "Could not get collateral utxo." queryCollateralUtxo
      )
      pure

queryCollateralUtxo :: Contract (Maybe Utxo)
queryCollateralUtxo =
  getWalletUtxos >>=
    maybe (pure Nothing)
      ( pure <<< Array.find (isCollateralTxOut <<< _.output <<< unwrap <<< snd) <<<
          Map.toUnfoldable
      )

isCollateralTxOut :: TransactionOutput -> Boolean
isCollateralTxOut (TransactionOutput txOut) =
  txOut.amount == Value.lovelaceValueOf appConst.collateralLovelace
    && (txOut.datum == NoOutputDatum)
    && isNothing txOut.referenceScript

createCollateralUtxo :: Contract Unit
createCollateralUtxo = do
  ownPkh <- liftedM "Could not get own pkh." ownPaymentPubKeyHash
  { txHash } <- submitTxReturningContractResult {} $ emptySubmitTxData
    { constraints =
        Constraints.mustPayToPubKey ownPkh $
          Value.lovelaceValueOf appConst.collateralLovelace
    }
  awaitTxConfirmed txHash
