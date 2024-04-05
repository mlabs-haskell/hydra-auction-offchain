module Test.Contract.StartBidding
  ( startBidding
  , suite
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.Test (ContractTest, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Control.Monad.Except (runExceptT)
import HydraAuctionOffchain.Contract (mkStartBiddingContractWithErrors)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended(AuctionInfoExtended)
  , ContractResult
  )
import Mote (group, test)
import Test.Contract.AnnounceAuction (announceAuction)
import Test.Helpers (defDistribution, waitUntil)

suite :: TestPlanM ContractTest Unit
suite =
  group "start-bidding" do
    test "seller enables bidding at the auction" do
      withWallets defDistribution \seller ->
        withKeyWallet seller do
          { txHash, auctionInfo } <- announceAuction
          awaitTxConfirmed txHash
          void $ startBidding auctionInfo

startBidding :: AuctionInfoExtended -> Contract ContractResult
startBidding auctionInfo@(AuctionInfoExtended { auctionTerms }) = do
  waitUntil (unwrap auctionTerms).biddingStart
  liftedE $ runExceptT $
    mkStartBiddingContractWithErrors (wrap { auctionInfo })
