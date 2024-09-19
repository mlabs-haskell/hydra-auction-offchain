module Test.Contract.DelegateServer
  ( suite
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address (fromCardano) as Plutus.Address
import Cardano.Types (BigNum)
import Cardano.Types.BigNum (fromInt) as BigNum
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.Test (ContractTest, withKeyWallet)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (untilJust)
import Control.Parallel (parTraverse_)
import Ctl.Internal.Contract.Hooks (ClusterParameters)
import Data.Array (foldRecM, last, replicate, singleton, snoc) as Array
import Data.Time.Duration (Seconds(Seconds))
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
  ( HydraHeadStatus
      ( HeadStatus_Idle
      , HeadStatus_Initializing
      , HeadStatus_Open
      , HeadStatus_Closed
      , HeadStatus_Final
      )
  )
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  )
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import HydraAuctionOffchain.Contract (discoverBidders)
import HydraAuctionOffchain.Contract.QueryUtxo (queryStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended
  , BidTerms(BidTerms)
  , BidderInfo(BidderInfo)
  , StandingBidState(StandingBidState)
  , bidderSignatureMessage
  )
import HydraAuctionOffchain.Helpers (mkPosixTimeUnsafe, randomElem, waitSeconds)
import HydraAuctionOffchain.Wallet (signMessage)
import Mote (group, skip, test)
import Partial.Unsafe (unsafePartial)
import Test.Contract.AnnounceAuction (AuctionTermsMutator, announceAuctionFix)
import Test.Contract.AuthorizeBidders (authorizeBidders)
import Test.Contract.EnterAuction (enterAuction, genValidBidderDeposit)
import Test.Contract.Fixtures (minBidIncrementFixture, startingBidFixture)
import Test.Contract.PlaceBid (discoverSellerSignature)
import Test.Contract.StartBidding (startBidding)
import Test.DelegateServer.Cluster (TestAppHandle, withWallets')
import Test.Helpers (defDistribution, untilM, waitUntil)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSampleOne)
import Test.Spec.Assertions (shouldEqual, shouldReturn, shouldSatisfy)

suite :: Ref ClusterParameters -> TestPlanM ContractTest Unit
suite clusterParams = do
  group "delegate-server" do
    test "delegates set announced auction as active auction" do
      withWallets' clusterParams defDistribution
        \seller delegates withDelegateServerCluster -> do
          { txHash, auctionInfo } <-
            withKeyWallet seller $
              announceAuctionFix (_ { delegates = delegates })
          awaitTxConfirmed txHash

          withDelegateServerCluster auctionInfo \appHandle ->
            shouldEqual auctionInfo =<<
              untilJust do
                waitSeconds one
                appHandle.getActiveAuction

    -- NOTE: All nodes in the hydra head fail to submit an InitTx
    -- transaction with NotEnoughFuel error when raiseExUnitsToMax
    -- parameter of PlutipConfig is set to true.
    test "delegates initialize head at bidding start time" do
      withWallets' clusterParams defDistribution
        \seller delegates withDelegateServerCluster -> do
          { txHash, auctionInfo } <-
            withKeyWallet seller $
              announceAuctionFix (_ { delegates = delegates })
          awaitTxConfirmed txHash

          withDelegateServerCluster auctionInfo \appHandle -> do
            let biddingStart = (unwrap (unwrap auctionInfo).auctionTerms).biddingStart
            waitUntil biddingStart
            untilM (eq HeadStatus_Initializing)
              appHandle.getHeadStatus

    test "delegates close head at bidding end time" do
      placeL2Bids clusterParams (validBids { maxNumBids: 2 }) shortBiddingPeriod \rec -> do
        let biddingEnd = (unwrap (unwrap rec.auctionInfo).auctionTerms).biddingEnd
        waitUntil biddingEnd
        untilM (eq HeadStatus_Closed)
          rec.appHandle.getHeadStatus

    test "delegates move bid back to L1 after contestation period" do
      placeL2Bids clusterParams (validBids { maxNumBids: 2 }) shortBiddingPeriod \rec -> do
        let biddingEnd = (unwrap (unwrap rec.auctionInfo).auctionTerms).biddingEnd
        waitUntil biddingEnd
        untilM (eq HeadStatus_Final)
          rec.appHandle.getHeadStatus
        let lastBid = Array.last rec.bids <#> StandingBidState <<< Just
        untilM (eq lastBid <<< map snd) do
          queryStandingBidUtxo (unwrap rec.auctionInfo)

    group "move-bid" do
      -- FIXME: Flaky, skipped because of the race condition:
      -- Delegates initialize the head before the moveBid request is made,
      -- resulting in an unexpected response.
      skip $ test "delegates move standing bid to L2 on request (head idle)" do
        moveBidTest clusterParams { autoInit: false }

      test "delegates move standing bid to L2 on request (head initializing)" do
        moveBidTest clusterParams { autoInit: true }

    group "place-bid-l2" do
      test "bidders place multiple valid bids on L2" do
        placeBidTest clusterParams $ validBids { maxNumBids: 10 }

      test "bidder cannot place bid below starting bid" do
        placeBidTest clusterParams
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
        placeBidTest clusterParams
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

shortBiddingPeriod :: AuctionTermsMutator
shortBiddingPeriod auctionTerms =
  auctionTerms
    { biddingEnd =
        auctionTerms.biddingStart + mkPosixTimeUnsafe (Seconds 30.0)
    }

openHead :: TestAppHandle -> { autoInit :: Boolean } -> Contract Unit
openHead appHandle { autoInit } = do
  if autoInit then do
    untilM (eq HeadStatus_Initializing)
      appHandle.getHeadStatus
    appHandle.moveBidToL2 >>=
      flip shouldSatisfy case _ of
        ServerResponseSuccess (MoveBidSuccess_CommittedStandingBid _) -> true
        _ -> false
  else do
    untilM (eq HeadStatus_Idle)
      appHandle.getHeadStatus
    appHandle.moveBidToL2 `shouldReturn`
      ServerResponseSuccess MoveBidSuccess_SentInitHeadRequest
  untilM (eq HeadStatus_Open)
    appHandle.getHeadStatus

moveBidTest :: Ref ClusterParameters -> { autoInit :: Boolean } -> ContractTest
moveBidTest clusterParams { autoInit } =
  withWallets' clusterParams defDistribution
    \seller delegates withDelegateServerCluster -> do
      auctionInfo <- withKeyWallet seller do
        { txHash: announceTxHash, auctionInfo } <-
          announceAuctionFix (_ { delegates = delegates })
        awaitTxConfirmed announceTxHash
        { txHash: startBiddingTxHash } <- startBidding auctionInfo
        awaitTxConfirmed startBiddingTxHash
        pure auctionInfo

      withDelegateServerCluster auctionInfo \appHandle -> do
        openHead appHandle { autoInit }
        untilM (eq (Just $ StandingBidState Nothing))
          appHandle.queryStandingBidL2

mkBidTerms :: AuctionInfoExtended -> BigNum -> Contract BidTerms
mkBidTerms auctionInfo bidAmount = do
  sellerSignature <- discoverSellerSignature auctionInfo Nothing
  pkh <- liftedM "Could not get bidder pkh" ownPaymentPubKeyHash
  let payload = bidderSignatureMessage (unwrap auctionInfo).auctionId (unwrap pkh) bidAmount
  { signature, vkey, address } <- liftedE $ runExceptT $ signMessage payload
  bidderAddress <-
    liftMaybe (error "Could not convert bidder address to Plutus.Address.") $
      Plutus.Address.fromCardano address
  pure $ BidTerms
    { bidder: BidderInfo { bidderAddress, bidderVk: vkey }
    , price: bidAmount
    , bidderSignature: signature
    , sellerSignature
    }

type PlaceBidTestParams =
  { numBidders :: Int
  , genBids :: Gen (Array { bidAmount :: Int, valid :: Boolean })
  }

type L2TestContData =
  { auctionInfo :: AuctionInfoExtended
  , appHandle :: TestAppHandle
  , bids :: Array BidTerms
  }

validBids :: { maxNumBids :: Int } -> PlaceBidTestParams
validBids { maxNumBids } =
  { numBidders: maxNumBids
  , genBids: do
      numBids <- chooseInt one maxNumBids
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

placeBidTest :: Ref ClusterParameters -> PlaceBidTestParams -> ContractTest
placeBidTest clusterParams params =
  placeL2Bids clusterParams params identity (const (pure unit))

placeL2Bids
  :: Ref ClusterParameters
  -> PlaceBidTestParams
  -> AuctionTermsMutator
  -> (L2TestContData -> Contract Unit)
  -> ContractTest
placeL2Bids clusterParams params fixAuctionTerms cont =
  withWallets' clusterParams
    (defDistribution /\ Array.replicate params.numBidders defDistribution)
    \(seller /\ bidderKws) delegates withDelegateServerCluster -> do
      auctionInfo <-
        withKeyWallet seller do
          res <- announceAuctionFix (_ { delegates = delegates } <<< fixAuctionTerms)
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
        bidTermsList <- traverse
          ( \{ bidAmount, valid } -> do
              bidder <- randomElem bidderKws
              withKeyWallet bidder do
                bidTerms <- mkBidTerms auctionInfo $ BigNum.fromInt bidAmount
                if valid then do
                  appHandle.placeBidL2 bidTerms `shouldReturn`
                    ServerResponseSuccess PlaceBidSuccess_SubmittedTransaction
                  untilM (eq (Just $ StandingBidState $ Just bidTerms))
                    appHandle.queryStandingBidL2
                else do
                  appHandle.placeBidL2 bidTerms `shouldReturn`
                    ServerResponseError
                      (PlaceBidError_ContractError PlaceBidL2_Error_InvalidBidStateTransition)
                pure bidTerms
          )
          bids
        cont
          { auctionInfo
          , appHandle
          , bids: bidTermsList
          }
