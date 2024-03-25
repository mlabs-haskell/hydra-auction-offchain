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
import Data.Argonaut (encodeJson)
import Data.Lens ((.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (unwrap)
import DelegateServer.App (runContract)
import DelegateServer.HydraNodeApi.Http (commit)
import DelegateServer.HydraNodeApi.Types.Commit
  ( CommitUtxoMap
  , mkCollateralCommit
  , mkStandingBidCommit
  )
import DelegateServer.Lib.ServerConfig (mkLocalhostHttpServerConfig)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.State (class AppBase, class AppInit, access, readAppState)
import HydraAuctionOffchain.Contract.QueryUtxo (queryStandingBidUtxo)
import HydraAuctionOffchain.Contract.Validators (mkStandingBidValidator)
import HydraAuctionOffchain.Lib.Json (printJson)
import Type.Proxy (Proxy(Proxy))

commitStandingBid :: forall m. AppInit m => m Unit
commitStandingBid = do
  auctionInfo <- unwrap <$> readAppState (Proxy :: _ "auctionInfo")
  collateralUtxo <- readAppState (Proxy :: _ "collateralUtxo")
  standingBidValidator <- runContract $ mkStandingBidValidator auctionInfo.auctionId
    auctionInfo.auctionTerms
  bidUtxo <- runContract $ queryStandingBidUtxo auctionInfo
  case bidUtxo of
    Just (utxo /\ _) | Just bidCommit <- mkStandingBidCommit utxo standingBidValidator -> do
      liftEffect $ log $ "Ready to commit standing bid: " <>
        printJson (encodeJson bidCommit)
      commitUtxos $ bidCommit <> mkCollateralCommit collateralUtxo
    _ -> pure unit

commitCollateral :: forall m. AppInit m => m Unit
commitCollateral =
  (commitUtxos <<< mkCollateralCommit)
    =<< readAppState (Proxy :: _ "collateralUtxo")

commitUtxos :: forall m. AppBase m => CommitUtxoMap -> m Unit
commitUtxos utxos = do
  { hydraNodeApi, cardanoSk } <- unwrap <$> access (Proxy :: _ "config")
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
