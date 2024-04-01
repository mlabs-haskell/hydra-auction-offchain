module Test.Contract.PlaceBid
  ( discoverSellerSignature
  , suite
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Test (ContractTest, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Control.Monad.Except (runExceptT)
import HydraAuctionOffchain.Contract
  ( discoverBidders
  , discoverSellerSignatureWithErrors
  , mkPlaceBidContractWithErrors
  )
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended(AuctionInfoExtended)
  , ContractResult
  )
import Mote (group, test)
import Test.Contract.AnnounceAuction (announceAuction)
import Test.Contract.AuthorizeBidders (authorizeBidders)
import Test.Contract.EnterAuction (enterAuction, genValidBidderDeposit)
import Test.Contract.StartBidding (startBidding)
import Test.Helpers (defDistribution)
import Test.QuickCheck.Gen (randomSampleOne)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM ContractTest Unit
suite =
  group "place-bid" do
    test "bidder places a starting bid" do
      withWallets (defDistribution /\ defDistribution) \(seller /\ bidder) -> do
        { txHash: announceTxHash, auctionInfo } <-
          withKeyWallet seller do
            announceAuction Nothing
        awaitTxConfirmed announceTxHash

        { txHash: enterTxHash } <-
          withKeyWallet bidder do
            depositAmount <- liftEffect $ randomSampleOne $ genValidBidderDeposit
              auctionInfo
            enterAuction auctionInfo depositAmount
        awaitTxConfirmed enterTxHash

        bidders <- discoverBidders auctionInfo
        shouldEqual (length bidders) 1

        withKeyWallet seller do
          { txHash: authTxHash } <- authorizeBidders auctionInfo bidders
          { txHash: startBiddingTxHash } <- startBidding auctionInfo
          awaitTxConfirmed authTxHash
          awaitTxConfirmed startBiddingTxHash

        withKeyWallet bidder do
          sellerSignature <- discoverSellerSignature auctionInfo
          void $ placeStartingBid auctionInfo sellerSignature

discoverSellerSignature :: AuctionInfoExtended -> Contract ByteArray
discoverSellerSignature (AuctionInfoExtended auctionInfo) = do
  sellerSignature <- liftedE $ runExceptT $
    discoverSellerSignatureWithErrors
      ( wrap
          { auctionCs: auctionInfo.auctionId
          , sellerAddress: (unwrap auctionInfo.auctionTerms).sellerAddress
          }
      )
  liftContractM "Could not get seller signature."
    sellerSignature

placeStartingBid :: AuctionInfoExtended -> ByteArray -> Contract ContractResult
placeStartingBid auctionInfo sellerSignature =
  liftedE $ runExceptT $
    mkPlaceBidContractWithErrors
      ( wrap
          { auctionInfo
          , sellerSignature
          , bidAmount: (unwrap (unwrap auctionInfo).auctionTerms).startingBid
          }
      )
