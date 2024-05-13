module Test.Contract.AuthorizeBidders
  ( authorizeBidders
  , suite
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.Test (ContractTest, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Control.Monad.Except (runExceptT)
import Data.Foldable (length)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Contract
  ( BidderInfoCandidate
  , discoverBidders
  , mkAuthorizeBiddersContractWithErrors
  )
import HydraAuctionOffchain.Contract.Types (AuctionInfoExtended, ContractResult)
import Mote (group, test)
import Test.Contract.AnnounceAuction (announceAuction)
import Test.Contract.EnterAuction
  ( enterAuction
  , genInvalidBidderDeposit
  , genValidBidderDeposit
  )
import Test.Helpers (defDistribution)
import Test.QuickCheck.Gen (randomSampleOne)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM ContractTest Unit
suite =
  group "authorize-bidders" do
    test "seller authorizes bidders" do
      withWallets (defDistribution /\ defDistribution /\ defDistribution)
        \(seller /\ bidder0 /\ bidder1) -> do
          { txHash: announceTxHash, auctionInfo } <-
            withKeyWallet seller do
              announceAuction
          awaitTxConfirmed announceTxHash

          { txHash: enterTxHash0 } <-
            withKeyWallet bidder0 do
              depositAmount <- liftEffect $ randomSampleOne $ genValidBidderDeposit
                auctionInfo
              enterAuction auctionInfo depositAmount
          { txHash: enterTxHash1 } <-
            withKeyWallet bidder1 do
              depositAmount <- liftEffect $ randomSampleOne $ genInvalidBidderDeposit
                auctionInfo
              enterAuction auctionInfo depositAmount
          awaitTxConfirmed enterTxHash0
          awaitTxConfirmed enterTxHash1

          bidders <- discoverBidders auctionInfo
          shouldEqual (length bidders) 2

          withKeyWallet seller do
            void $ authorizeBidders auctionInfo bidders

authorizeBidders
  :: AuctionInfoExtended
  -> Array BidderInfoCandidate
  -> Contract ContractResult
authorizeBidders auctionInfo bidders = do
  let biddersToAuthorize = bidders <#> _.bidderVk <<< unwrap <<< _.bidderInfo <<< unwrap
  liftedE $ runExceptT $
    mkAuthorizeBiddersContractWithErrors
      ( wrap
          { auctionCs: (unwrap auctionInfo).auctionId
          , biddersToAuthorize
          }
      )
