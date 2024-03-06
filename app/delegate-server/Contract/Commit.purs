module DelegateServer.Contract.Commit
  ( commitCollateral
  , commitStandingBid
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , _vkeys
  , _witnessSet
  , signTransaction
  , submit
  )
import Control.Monad.Reader (asks)
import Data.Argonaut (encodeJson)
import Data.Lens ((.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import DelegateServer.HydraNodeApi.Http (commit)
import DelegateServer.HydraNodeApi.Types.Commit
  ( CommitUtxoMap
  , mkCollateralCommit
  , mkStandingBidCommit
  )
import DelegateServer.Lib.Json (printJson)
import DelegateServer.Lib.ServerConfig (mkLocalhostHttpServerConfig)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.State (AppM, readAppState, runContract)
import HydraAuctionOffchain.Contract.QueryUtxo (queryStandingBidUtxo)
import HydraAuctionOffchain.Contract.Validators (mkStandingBidValidator)

commitStandingBid :: AppM Unit
commitStandingBid = do
  auctionInfo <- unwrap <$> readAppState _.auctionInfo
  collateralUtxo <- readAppState _.collateralUtxo
  standingBidValidator <- runContract $ mkStandingBidValidator auctionInfo.auctionId
    auctionInfo.auctionTerms
  bidUtxo <- runContract $ queryStandingBidUtxo auctionInfo
  case bidUtxo of
    Just (utxo /\ _) | Just bidCommit <- mkStandingBidCommit utxo standingBidValidator -> do
      liftEffect $ log $ "Ready to commit standing bid: " <>
        printJson (encodeJson bidCommit)
      commitUtxos $ bidCommit <> mkCollateralCommit collateralUtxo
    _ -> pure unit

commitCollateral :: AppM Unit
commitCollateral =
  (commitUtxos <<< mkCollateralCommit) =<< readAppState _.collateralUtxo

commitUtxos :: CommitUtxoMap -> AppM Unit
commitUtxos utxos = do
  { hydraNodeApi, cardanoSk } <- asks _.config
  let serverConfig = mkLocalhostHttpServerConfig hydraNodeApi.port
  commitRes <- liftAff $ commit serverConfig utxos
  case commitRes of
    Left err -> liftEffect $ log $ show err
    Right draftCommitTx ->
      runContract do
        let commitTx = BalancedSignedTransaction draftCommitTx.cborHex
        signedTx <- (withWallet cardanoSk <<< signTransaction) =<< reSignTransaction commitTx
        txHash <- submit signedTx
        liftEffect $ log $ "Submitted commit tx: " <> show txHash <> "."

reSignTransaction :: BalancedSignedTransaction -> Contract BalancedSignedTransaction
reSignTransaction tx =
  signTransaction
    (tx # simple _Newtype <<< _witnessSet <<< _vkeys .~ Nothing)
