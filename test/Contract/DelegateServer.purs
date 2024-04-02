module Test.Contract.DelegateServer
  ( suite
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE, liftedM)
import Contract.Test (ContractTest, withKeyWallet)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (untilJust)
import DelegateServer.Handlers.MoveBid
  ( MoveBidSuccess(MoveBidSuccess_SentInitHeadRequest, MoveBidSuccess_CommittedStandingBid)
  )
import DelegateServer.Handlers.PlaceBid (PlaceBidSuccess(PlaceBidSuccess_SubmittedTransaction))
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus(HeadStatus_Idle, HeadStatus_Initializing, HeadStatus_Open)
  )
import DelegateServer.Types.ServerResponse (ServerResponse(ServerResponseSuccess))
import HydraAuctionOffchain.Contract (discoverBidders)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended(AuctionInfoExtended)
  , BidTerms(BidTerms)
  , BidderInfo(BidderInfo)
  , StandingBidState(StandingBidState)
  , bidderSignatureMessage
  )
import HydraAuctionOffchain.Helpers (waitSeconds)
import HydraAuctionOffchain.Wallet (signMessage)
import Mote (group, skip, test)
import Test.Contract.AnnounceAuction (announceAuction)
import Test.Contract.AuthorizeBidders (authorizeBidders)
import Test.Contract.EnterAuction (enterAuction, genValidBidderDeposit)
import Test.Contract.PlaceBid (discoverSellerSignature)
import Test.Contract.StartBidding (startBidding)
import Test.DelegateServer.Cluster (TestAppHandle, withWallets')
import Test.Helpers (defDistribution, untilM, waitUntil)
import Test.QuickCheck.Gen (randomSampleOne)
import Test.Spec.Assertions (shouldEqual, shouldReturn, shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite =
  group "delegate-server" do
    test "delegates set announced auction as active auction" do
      withWallets' defDistribution
        \seller delegates withDelegateServerCluster -> do
          { txHash, auctionInfo } <- withKeyWallet seller $ announceAuction $ Just delegates
          awaitTxConfirmed txHash

          withDelegateServerCluster auctionInfo \appHandle ->
            shouldEqual auctionInfo =<<
              untilJust do
                waitSeconds one
                appHandle.getActiveAuction

    -- NOTE: All nodes in the hydra head fail to submit an InitTx
    -- transaction with NotEnoughFuel error when raiseExUnitsToMax
    -- parameter of PlutipConfig is set to true.
    test "delegates initialize a hydra head at bidding start time" do
      withWallets' defDistribution
        \seller delegates withDelegateServerCluster -> do
          { txHash, auctionInfo } <- withKeyWallet seller $ announceAuction $ Just delegates
          awaitTxConfirmed txHash

          withDelegateServerCluster auctionInfo \appHandle -> do
            let biddingStart = (unwrap (unwrap auctionInfo).auctionTerms).biddingStart
            waitUntil biddingStart
            waitSeconds 5
            appHandle.getHeadStatus
              `shouldReturn` HeadStatus_Initializing

    group "move-bid" do
      -- FIXME: Flaky, skipped because of the race condition:
      -- Delegates initialize the head before the moveBid request is made,
      -- resulting in an unexpected response.
      skip $ test "delegates move standing bid to L2 on request (head idle)" do
        moveBidTest { autoInit: false }

      test "delegates move standing bid to L2 on request (head initializing)" do
        moveBidTest { autoInit: true }

    group "place-bid-l2" do
      test "valid transition from empty standing bid" do
        withWallets' (defDistribution /\ defDistribution)
          \(seller /\ bidder) delegates withDelegateServerCluster -> do
            auctionInfo@(AuctionInfoExtended auctionInfoRec) <-
              withKeyWallet seller do
                res <- announceAuction $ Just delegates
                awaitTxConfirmed res.txHash
                pure res.auctionInfo

            withKeyWallet bidder do
              depositAmount <- liftEffect $ randomSampleOne $ genValidBidderDeposit
                auctionInfo
              { txHash } <- enterAuction auctionInfo depositAmount
              awaitTxConfirmed txHash

            bidders <- discoverBidders auctionInfo
            shouldEqual (length bidders) 1
            withKeyWallet seller do
              { txHash: authTxHash } <- authorizeBidders auctionInfo bidders
              { txHash: startBiddingTxHash } <- startBidding auctionInfo
              awaitTxConfirmed authTxHash
              awaitTxConfirmed startBiddingTxHash

            withDelegateServerCluster auctionInfo \appHandle -> do
              openHead appHandle { autoInit: true }
              untilM (eq (Just $ StandingBidState Nothing))
                appHandle.queryStandingBidL2

              withKeyWallet bidder do
                sellerSignature <- discoverSellerSignature auctionInfo
                pkh <- liftedM "Could not get bidder pkh" ownPaymentPubKeyHash
                let
                  bidAmount = (unwrap auctionInfoRec.auctionTerms).startingBid
                  payload = bidderSignatureMessage auctionInfoRec.auctionId (unwrap pkh)
                    bidAmount
                { signature, vkey, address } <- liftedE $ runExceptT $ signMessage payload
                let
                  bidTerms = BidTerms
                    { bidder: BidderInfo { bidderAddress: address, bidderVk: vkey }
                    , price: bidAmount
                    , bidderSignature: signature
                    , sellerSignature: sellerSignature
                    }
                appHandle.placeBidL2 bidTerms `shouldReturn`
                  ServerResponseSuccess PlaceBidSuccess_SubmittedTransaction
                untilM (eq (Just $ StandingBidState $ Just bidTerms))
                  appHandle.queryStandingBidL2

moveBidTest :: { autoInit :: Boolean } -> ContractTest
moveBidTest { autoInit } =
  withWallets' defDistribution
    \seller delegates withDelegateServerCluster -> do
      auctionInfo <- withKeyWallet seller do
        { txHash: announceTxHash, auctionInfo } <- announceAuction $ Just delegates
        awaitTxConfirmed announceTxHash
        { txHash: startBiddingTxHash } <- startBidding auctionInfo
        awaitTxConfirmed startBiddingTxHash
        pure auctionInfo

      withDelegateServerCluster auctionInfo \appHandle -> do
        openHead appHandle { autoInit }
        untilM (eq (Just $ StandingBidState Nothing))
          appHandle.queryStandingBidL2

openHead :: TestAppHandle -> { autoInit :: Boolean } -> Contract Unit
openHead appHandle { autoInit } = do
  if autoInit then do
    untilM (eq HeadStatus_Initializing)
      appHandle.getHeadStatus
    appHandle.moveBidToL2 >>=
      flip shouldSatisfy case _ of
        ServerResponseSuccess (MoveBidSuccess_CommittedStandingBid _ _) -> true
        _ -> false
  else do
    untilM (eq HeadStatus_Idle)
      appHandle.getHeadStatus
    appHandle.moveBidToL2 `shouldReturn`
      ServerResponseSuccess MoveBidSuccess_SentInitHeadRequest
  untilM (eq HeadStatus_Open)
    appHandle.getHeadStatus
