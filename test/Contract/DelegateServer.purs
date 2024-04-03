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
import Control.Parallel (parTraverse_)
import Data.Array (foldRecM, last, replicate, singleton, snoc) as Array
import DelegateServer.Contract.PlaceBid
  ( PlaceBidL2ContractError(PlaceBidL2_Error_InvalidBidStateTransition)
  )
import DelegateServer.Handlers.MoveBid
  ( MoveBidSuccess(MoveBidSuccess_SentInitHeadRequest, MoveBidSuccess_CommittedStandingBid)
  )
import DelegateServer.Handlers.PlaceBid
  ( PlaceBidError(PlaceBidError_ContractError)
  , PlaceBidSuccess(PlaceBidSuccess_SubmittedTransaction)
  )
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus(HeadStatus_Idle, HeadStatus_Initializing, HeadStatus_Open)
  )
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  )
import HydraAuctionOffchain.Contract (discoverBidders)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended
  , BidTerms(BidTerms)
  , BidderInfo(BidderInfo)
  , StandingBidState(StandingBidState)
  , bidderSignatureMessage
  )
import HydraAuctionOffchain.Helpers (waitSeconds)
import HydraAuctionOffchain.Wallet (signMessage)
import JS.BigInt (BigInt, fromInt)
import Mote (group, skip, test)
import Partial.Unsafe (unsafePartial)
import Test.Contract.AnnounceAuction (announceAuction)
import Test.Contract.AuthorizeBidders (authorizeBidders)
import Test.Contract.EnterAuction (enterAuction, genValidBidderDeposit)
import Test.Contract.Fixtures (minBidIncrementFixture, startingBidFixture)
import Test.Contract.PlaceBid (discoverSellerSignature)
import Test.Contract.StartBidding (startBidding)
import Test.DelegateServer.Cluster (TestAppHandle, withWallets')
import Test.Helpers (defDistribution, randomElem, untilM, waitUntil)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSampleOne)
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
      test "bidders place multiple valid bids on L2" do
        placeBidTest
          { numBidders: 10
          , genBids: do
              numBids <- chooseInt 1 10
              let lastUnsafe = unsafePartial fromJust <<< Array.last
              Array.foldRecM
                ( \bids _ -> do
                    bidIncr <- chooseInt minBidIncrementFixture (2 * minBidIncrementFixture)
                    pure $ Array.snoc bids
                      { bidAmount: (lastUnsafe bids).bidAmount + bidIncr
                      , valid: true
                      }
                )
                (Array.singleton { bidAmount: startingBidFixture, valid: true })
                (Array.replicate (numBids - 1) unit)
          }

      test "bidder cannot place bid below starting bid" do
        placeBidTest
          { numBidders: one
          , genBids: do
              invalidBidAmount <- chooseInt zero $ startingBidFixture - one
              pure
                [ { bidAmount: invalidBidAmount
                  , valid: false
                  }
                ]
          }

      test "bidder cannot place bid with invalid increment" do
        placeBidTest
          { numBidders: one
          , genBids: do
              invalidBidIncr <- chooseInt zero $ minBidIncrementFixture - one
              pure
                [ { bidAmount: startingBidFixture
                  , valid: true
                  }
                , { bidAmount: startingBidFixture + invalidBidIncr
                  , valid: false
                  }
                ]
          }

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

mkBidTerms :: AuctionInfoExtended -> BigInt -> Contract BidTerms
mkBidTerms auctionInfo bidAmount = do
  sellerSignature <- discoverSellerSignature auctionInfo
  pkh <- liftedM "Could not get bidder pkh" ownPaymentPubKeyHash
  let payload = bidderSignatureMessage (unwrap auctionInfo).auctionId (unwrap pkh) bidAmount
  { signature, vkey, address } <- liftedE $ runExceptT $ signMessage payload
  pure $ BidTerms
    { bidder: BidderInfo { bidderAddress: address, bidderVk: vkey }
    , price: bidAmount
    , bidderSignature: signature
    , sellerSignature
    }

type PlaceBidTestParams =
  { numBidders :: Int
  , genBids :: Gen (Array { bidAmount :: Int, valid :: Boolean })
  }

placeBidTest :: PlaceBidTestParams -> ContractTest
placeBidTest params =
  withWallets' (defDistribution /\ Array.replicate params.numBidders defDistribution)
    \(seller /\ bidderKws) delegates withDelegateServerCluster -> do
      auctionInfo <-
        withKeyWallet seller do
          res <- announceAuction $ Just delegates
          awaitTxConfirmed res.txHash
          pure res.auctionInfo

      parTraverse_
        ( \bidder ->
            withKeyWallet bidder do
              depositAmount <- liftEffect $ randomSampleOne $ genValidBidderDeposit
                auctionInfo
              { txHash } <- enterAuction auctionInfo depositAmount
              awaitTxConfirmed txHash
        )
        bidderKws

      bidders <- discoverBidders auctionInfo
      withKeyWallet seller do
        { txHash: authTxHash } <- authorizeBidders auctionInfo bidders
        { txHash: startBiddingTxHash } <- startBidding auctionInfo
        awaitTxConfirmed authTxHash
        awaitTxConfirmed startBiddingTxHash

      withDelegateServerCluster auctionInfo \appHandle -> do
        openHead appHandle { autoInit: true }
        untilM (eq (Just $ StandingBidState Nothing))
          appHandle.queryStandingBidL2

        bids <- liftEffect $ randomSampleOne params.genBids
        traverse_
          ( \{ bidAmount, valid } -> do
              bidder <- randomElem bidderKws
              withKeyWallet bidder do
                bidTerms <- mkBidTerms auctionInfo $ fromInt bidAmount
                if valid then do
                  appHandle.placeBidL2 bidTerms `shouldReturn`
                    ServerResponseSuccess PlaceBidSuccess_SubmittedTransaction
                  untilM (eq (Just $ StandingBidState $ Just bidTerms))
                    appHandle.queryStandingBidL2
                else do
                  appHandle.placeBidL2 bidTerms `shouldReturn`
                    ServerResponseError
                      (PlaceBidError_ContractError PlaceBidL2_Error_InvalidBidStateTransition)
          )
          bids
