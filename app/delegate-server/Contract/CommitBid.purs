module DelegateServer.Contract.CommitBid
  ( commitStandingBid
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
import DelegateServer.HydraNodeApi.Types.Commit (mkStandingBidCommit)
import DelegateServer.Lib.Json (printJson)
import DelegateServer.Lib.ServerConfig (mkLocalhostHttpServerConfig)
import DelegateServer.State (AppM, askAuctionInfo, runContract)
import HydraAuctionOffchain.Contract.QueryUtxo (queryStandingBidUtxo)
import HydraAuctionOffchain.Contract.Validators (mkStandingBidValidator)

commitStandingBid :: AppM Unit
commitStandingBid = do
  auctionInfo <- unwrap <$> askAuctionInfo
  { hydraNodeApi } <- asks _.config
  runContract do
    standingBidValidator <- mkStandingBidValidator auctionInfo.auctionId
      auctionInfo.auctionTerms
    queryStandingBidUtxo auctionInfo >>= case _ of
      Just (utxo /\ _) | Just commitUtxo <- mkStandingBidCommit utxo standingBidValidator -> do
        liftEffect $ log $ "Ready to commit standing bid: " <>
          printJson (encodeJson commitUtxo)
        let serverConfig = mkLocalhostHttpServerConfig hydraNodeApi.port
        liftAff (commit serverConfig commitUtxo) >>= case _ of
          Right draftCommitTx -> do
            let commitTx = BalancedSignedTransaction draftCommitTx.cborHex
            signedTx <- reSignTransaction commitTx
            txHash <- submit signedTx
            liftEffect $ log $ "Submitted commit tx: " <> show txHash <> "."
          Left err -> liftEffect $ log $ show err
      _ -> pure unit

reSignTransaction :: BalancedSignedTransaction -> Contract BalancedSignedTransaction
reSignTransaction tx =
  signTransaction
    (tx # simple _Newtype <<< _witnessSet <<< _vkeys .~ Nothing)
