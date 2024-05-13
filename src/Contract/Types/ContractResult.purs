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

import Contract.AuxiliaryData (setGeneralTxMetadata)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder)
import Contract.CborBytes (cborByteLength)
import Contract.Log (logWarn')
import Contract.Metadata (GeneralTransactionMetadata)
import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction
  ( ExUnits
  , FinalizedTransaction
  , Redeemer
  , Transaction
  , TransactionHash
  , _witnessSet
  , balanceTxWithConstraints
  , getTxFinalFee
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Contract.UnbalancedTx (mkUnbalancedTx)
import Ctl.Internal.Cardano.Types.Transaction (_redeemers)
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.Foldable (foldl)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import JS.BigInt (BigInt)
import Prim.Row (class Nub, class Union) as Row
import Record (merge) as Record

type ContractResult = ContractResult' ()

type ContractResult' (extra :: Row Type) = Record (ContractResultRow extra)

type ContractResultRow (extra :: Row Type) =
  ( balancedSignedTx :: Transaction
  , txHash :: TransactionHash
  , txFinalFee :: BigInt
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

buildTx
  :: SubmitTxData
  -> Contract FinalizedTransaction
buildTx rec = do
  unbalancedTx' <- mkUnbalancedTx rec.lookups rec.constraints
  unbalancedTxWithMetadata <- traverse (setGeneralTxMetadata unbalancedTx') rec.metadata
  let unbalancedTx = fromMaybe unbalancedTx' unbalancedTxWithMetadata
  balancedTx <- balanceTxWithConstraints unbalancedTx rec.balancerConstraints
  pure balancedTx

submitTx
  :: forall (extra :: Row Type)
   . Row.Union extra (ContractResultRow ()) (ContractResultRow extra)
  => Row.Nub (ContractResultRow extra) (ContractResultRow extra)
  => Record extra
  -> FinalizedTransaction
  -> Contract (ContractResult' extra)
submitTx extra balancedTx = do
  balancedSignedTx <- signTransaction balancedTx
  txSize <- liftEffect $ getTxSize $ unwrap balancedSignedTx
  logWarn' $ "Tx size: " <> show txSize <> " bytes"
  let txExUnits = getTotalExUnits $ unwrap balancedSignedTx
  logWarn' $ "Ex units: " <> show txExUnits
  txHash <- submit balancedSignedTx
  pure $ Record.merge extra $
    { balancedSignedTx: unwrap balancedSignedTx
    , txHash
    , txFinalFee: getTxFinalFee balancedSignedTx
    , txExUnits
    , txSize
    }

getTxSize :: Transaction -> Effect Int
getTxSize = map (cborByteLength <<< toBytes) <<< convertTransaction

getTotalExUnits :: Transaction -> ExUnits
getTotalExUnits balancedSignedTx =
  mRedeemers # maybe emptyExUnits \redeemers ->
    foldl
      ( \acc red ->
          { mem: acc.mem + (unwrap red).exUnits.mem
          , steps: acc.steps + (unwrap red).exUnits.steps
          }
      )
      emptyExUnits
      redeemers
  where
  mRedeemers :: Maybe (Array Redeemer)
  mRedeemers = balancedSignedTx ^. _witnessSet <<< _redeemers

  emptyExUnits :: ExUnits
  emptyExUnits = { mem: zero, steps: zero }
