module DelegateServer.Contract.Collateral
  ( findCollateralUtxo
  , getCollateralUtxo
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedM)
import Contract.PlutusData (OutputDatum(NoOutputDatum))
import Contract.Transaction (TransactionOutput(TransactionOutput), awaitTxConfirmed)
import Contract.TxConstraints (mustPayToPubKey) as Constraints
import Contract.Utxos (UtxoMap)
import Contract.Value (lovelaceValueOf) as Value
import Contract.Wallet (getWalletAddress, getWalletUtxos, ownPaymentPubKeyHash)
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
queryCollateralUtxo = do
  utxos <- liftedM "Could not get wallet utxos." getWalletUtxos
  findCollateralUtxo utxos

findCollateralUtxo :: UtxoMap -> Contract (Maybe Utxo)
findCollateralUtxo utxos = do
  ownAddress <- liftedM "Could not get wallet address." getWalletAddress
  pure $ Map.toUnfoldable utxos # Array.find
    ( \utxo ->
        let
          txOut = _.output $ unwrap $ snd utxo
        in
          isCollateralTxOut txOut
            && ((unwrap txOut).address == ownAddress)
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
