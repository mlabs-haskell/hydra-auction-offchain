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
import Contract.CborBytes (cborByteLength)
import Contract.Log (logWarn')
import Contract.Metadata (GeneralTransactionMetadata)
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class IsData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts (class ValidatorTypes)
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
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.BigInt (BigInt)
import Data.Foldable (foldl)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
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

type SubmitTxData (validator :: Type) (redeemer :: Type) (datum :: Type) =
  { lookups :: ScriptLookups validator
  , constraints :: TxConstraints redeemer datum
  , balancerConstraints :: BalanceTxConstraintsBuilder
  , metadata :: Maybe GeneralTransactionMetadata
  }

emptySubmitTxData :: SubmitTxData Void Void Void
emptySubmitTxData =
  { lookups: mempty
  , constraints: mempty
  , balancerConstraints: mempty
  , metadata: Nothing
  }

submitTxReturningContractResult
  :: forall (validator :: Type) (datum :: Type) (redeemer :: Type) (extra :: Row Type)
   . ValidatorTypes validator datum redeemer
  => IsData datum
  => IsData redeemer
  => Row.Union extra (ContractResultRow ()) (ContractResultRow extra)
  => Row.Nub (ContractResultRow extra) (ContractResultRow extra)
  => Record extra
  -> SubmitTxData validator redeemer datum
  -> Contract (ContractResult' extra)
submitTxReturningContractResult extra rec = do
  unbalancedTx' <- liftedE $ mkUnbalancedTx rec.lookups rec.constraints
  unbalancedTxWithMetadata <- traverse (setGeneralTxMetadata unbalancedTx') rec.metadata
  let unbalancedTx = fromMaybe unbalancedTx' unbalancedTxWithMetadata
  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx rec.balancerConstraints
  balancedSignedTx <- signTransaction balancedTx
  txSize <- liftEffect $ getTxSize $ unwrap balancedSignedTx
  logWarn' $ "Tx size: " <> show txSize <> " bytes"
  txHash <- submit balancedSignedTx
  pure $ Record.merge extra $
    { balancedSignedTx: unwrap balancedSignedTx
    , txHash
    , txFinalFee: getTxFinalFee balancedSignedTx
    , txExUnits: getTotalExUnits $ unwrap balancedSignedTx
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
