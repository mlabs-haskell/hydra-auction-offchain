module HydraAuctionOffchain.Contract.Types.ContractResult
  ( ContractResult
  , ContractResult'
  , ContractResultRow
  , SubmitTxData
  , buildTx
  , emptySubmitTxData
  , getTotalExUnits
  , submitTx
  , submitTxReturningContractResult
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types (BigNum, ExUnits, Transaction, TransactionHash)
import Cardano.Types.Transaction (_body, _witnessSet)
import Cardano.Types.TransactionBody (_fee)
import Cardano.Types.TransactionWitnessSet (_redeemers)
import Contract.AuxiliaryData (setGeneralTxMetadata)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder)
import Contract.CborBytes (cborByteLength)
import Contract.Log (logWarn')
import Contract.Metadata (GeneralTransactionMetadata)
import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (balanceTx, signTransaction, submit)
import Contract.TxConstraints (TxConstraints)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Data.Foldable (foldMap)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Nub, class Union) as Row
import Record (merge) as Record

type ContractResult = ContractResult' ()

type ContractResult' (extra :: Row Type) = Record (ContractResultRow extra)

type ContractResultRow (extra :: Row Type) =
  ( balancedSignedTx :: Transaction
  , txHash :: TransactionHash
  , txFinalFee :: BigNum
  , txExUnits :: ExUnits
  , txSize :: Int
  | extra
  )

type SubmitTxData =
  { lookups :: ScriptLookups
  , constraints :: TxConstraints
  , balancerConstraints :: BalanceTxConstraintsBuilder
  , metadata :: Maybe GeneralTransactionMetadata
  }

emptySubmitTxData :: SubmitTxData
emptySubmitTxData =
  { lookups: mempty
  , constraints: mempty
  , balancerConstraints: mempty
  , metadata: Nothing
  }

submitTxReturningContractResult
  :: forall (extra :: Row Type)
   . Row.Union extra (ContractResultRow ()) (ContractResultRow extra)
  => Row.Nub (ContractResultRow extra) (ContractResultRow extra)
  => Record extra
  -> SubmitTxData
  -> Contract (ContractResult' extra)
submitTxReturningContractResult extra rec =
  submitTx extra =<< buildTx rec

buildTx :: SubmitTxData -> Contract Transaction
buildTx rec = do
  unbalancedTx /\ usedUtxos <- mkUnbalancedTx rec.lookups rec.constraints
  let
    unbalancedTxWithMetadata = setGeneralTxMetadata unbalancedTx <$> rec.metadata
    unbalancedTx' = fromMaybe unbalancedTx unbalancedTxWithMetadata
  balanceTx unbalancedTx' usedUtxos rec.balancerConstraints

submitTx
  :: forall (extra :: Row Type)
   . Row.Union extra (ContractResultRow ()) (ContractResultRow extra)
  => Row.Nub (ContractResultRow extra) (ContractResultRow extra)
  => Record extra
  -> Transaction
  -> Contract (ContractResult' extra)
submitTx extra balancedTx = do
  balancedSignedTx <- signTransaction balancedTx
  let txSize = cborByteLength $ encodeCbor balancedSignedTx
  logWarn' $ "Tx size: " <> show txSize <> " bytes"
  let txExUnits = getTotalExUnits balancedSignedTx
  logWarn' $ "Ex units: " <> show txExUnits
  txHash <- submit balancedSignedTx
  pure $ Record.merge extra $
    { balancedSignedTx
    , txHash
    , txFinalFee: unwrap $ balancedSignedTx ^. _body <<< _fee
    , txExUnits
    , txSize
    }

getTotalExUnits :: Transaction -> ExUnits
getTotalExUnits balancedSignedTx =
  unsafePartial do
    foldMap (_.exUnits <<< unwrap) $
      balancedSignedTx ^. _witnessSet <<< _redeemers
