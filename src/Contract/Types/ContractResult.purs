module HydraAuctionOffchain.Contract.Types.ContractResult
  ( ContractResult
  , ContractResult'
  , ContractResultRow
  , SubmitTxData
  , emptySubmitTxData
  , getTotalExUnits
  , submitTxReturningContractResult
  ) where

import Prelude

import Contract.AuxiliaryData (setGeneralTxMetadata)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder)
import Contract.Metadata (GeneralTransactionMetadata)
import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction
  ( ExUnits
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
import Data.Foldable (foldl)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
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
submitTxReturningContractResult extra rec = do
  unbalancedTx' <- mkUnbalancedTx rec.lookups rec.constraints
  unbalancedTxWithMetadata <- traverse (setGeneralTxMetadata unbalancedTx') rec.metadata
  let unbalancedTx = fromMaybe unbalancedTx' unbalancedTxWithMetadata
  balancedTx <- balanceTxWithConstraints unbalancedTx rec.balancerConstraints
  balancedSignedTx <- signTransaction balancedTx
  txHash <- submit balancedSignedTx
  pure $ Record.merge extra $
    { balancedSignedTx: unwrap balancedSignedTx
    , txHash
    , txFinalFee: getTxFinalFee balancedSignedTx
    , txExUnits: getTotalExUnits $ unwrap balancedSignedTx
    }

getTotalExUnits :: forall (extra :: Row Type). Transaction -> ExUnits
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
