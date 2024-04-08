module DelegateServer.Contract.Commit
  ( CommitCollateralError
      ( CommitCollateral_CommitRequestFailed
      )
  , CommitStandingBidError
      ( CommitBid_CouldNotFindStandingBidUtxo
      , CommitBid_CouldNotBuildCommit
      , CommitBid_CommitRequestFailed
      )
  , commitCollateral
  , commitStandingBid
  ) where

import Contract.Prelude

import Contract.Log (logDebug', logInfo')
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , TransactionHash
  , signTransaction
  , submit
  )
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT(ExceptT), withExceptT)
import Data.Argonaut (encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import DelegateServer.App (runContract, runContractLift)
import DelegateServer.HydraNodeApi.Http (commit)
import DelegateServer.HydraNodeApi.Types.Commit
  ( CommitUtxoMap
  , mkCollateralCommit
  , mkStandingBidCommit
  )
import DelegateServer.Lib.ServerConfig (mkLocalhostHttpServerConfig)
import DelegateServer.Lib.Transaction (reSignTransaction)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.State (class AppBase, class AppInit, access, readAppState)
import DelegateServer.Types.ServiceError (ServiceError)
import HydraAuctionOffchain.Contract.QueryUtxo (queryStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types (StandingBidState)
import HydraAuctionOffchain.Contract.Validators (mkStandingBidValidator)
import HydraAuctionOffchain.Lib.Json (printJson)
import Type.Proxy (Proxy(Proxy))

data CommitStandingBidError
  = CommitBid_CouldNotFindStandingBidUtxo
  | CommitBid_CouldNotBuildCommit
  | CommitBid_CommitRequestFailed ServiceError

derive instance Generic CommitStandingBidError _
derive instance Eq CommitStandingBidError

instance Show CommitStandingBidError where
  show = genericShow

commitStandingBid
  :: forall m
   . AppInit m
  => ExceptT CommitStandingBidError m
       (StandingBidState /\ TransactionHash)
commitStandingBid = do
  auctionInfo <- unwrap <$> readAppState (Proxy :: _ "auctionInfo")
  collateralUtxo <- readAppState (Proxy :: _ "collateralUtxo")
  standingBidValidator <-
    runContractLift $
      mkStandingBidValidator auctionInfo.auctionId auctionInfo.auctionTerms
  bidUtxo /\ standingBid <- runContract (queryStandingBidUtxo auctionInfo)
    !? CommitBid_CouldNotFindStandingBidUtxo
  bidCommit <- mkStandingBidCommit bidUtxo standingBidValidator
    ?? CommitBid_CouldNotBuildCommit
  logDebug' $ "Ready to commit standing bid: " <> printJson (encodeJson bidCommit)
  txHash <-
    withExceptT CommitBid_CommitRequestFailed
      (commitUtxos $ bidCommit <> mkCollateralCommit collateralUtxo)
  pure $ standingBid /\ txHash

data CommitCollateralError = CommitCollateral_CommitRequestFailed ServiceError

derive instance Generic CommitCollateralError _

instance Show CommitCollateralError where
  show = genericShow

commitCollateral :: forall m. AppInit m => ExceptT CommitCollateralError m TransactionHash
commitCollateral = do
  collateralUtxo <- readAppState (Proxy :: _ "collateralUtxo")
  withExceptT CommitCollateral_CommitRequestFailed
    (commitUtxos $ mkCollateralCommit collateralUtxo)

commitUtxos :: forall m. AppBase m => CommitUtxoMap -> ExceptT ServiceError m TransactionHash
commitUtxos utxos = do
  { hydraNodeApi, cardanoSk } <- unwrap <$> access (Proxy :: _ "config")
  let serverConfig = mkLocalhostHttpServerConfig hydraNodeApi.port
  draftCommitTx <- ExceptT $ liftAff $ commit serverConfig utxos
  runContractLift do
    let commitTx = BalancedSignedTransaction draftCommitTx.cborHex
    signedTx <-
      (withWallet cardanoSk <<< signTransaction)
        =<< reSignTransaction commitTx
    txHash <- submit signedTx
    logInfo' $ "Submitted commit tx: " <> show txHash <> "."
    pure txHash
