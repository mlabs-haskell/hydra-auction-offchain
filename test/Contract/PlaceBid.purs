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
import Contract.Wallet (KeyWallet)
import Control.Monad.Except (runExceptT)
import HydraAuctionOffchain.Contract
  ( discoverBidders
  , discoverSellerSignatureWithErrors
  , getWalletVkWithErrors
  , mkPlaceBidContractWithErrors
  )
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended(AuctionInfoExtended)
  , ContractResult
  , VerificationKey
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
    test "bidder places a starting bid (no bidder vk provided)" do
      withWallets (defDistribution /\ defDistribution) \(seller /\ bidder) -> do
        auctionInfo <- initAuction seller bidder
        withKeyWallet bidder do
          sellerSignature <- discoverSellerSignature auctionInfo Nothing
          void $ placeStartingBid auctionInfo sellerSignature

    test "bidder places a starting bid (bidder vk provided)" do
      withWallets (defDistribution /\ defDistribution) \(seller /\ bidder) -> do
        auctionInfo <- initAuction seller bidder
        withKeyWallet bidder do
          bidderVk <- liftedE $ runExceptT getWalletVkWithErrors
          sellerSignature <- discoverSellerSignature auctionInfo (Just bidderVk)
          void $ placeStartingBid auctionInfo sellerSignature

initAuction :: KeyWallet -> KeyWallet -> Contract AuctionInfoExtended
initAuction seller bidder = do
  auctionInfo <-
    withKeyWallet seller do
      res <- announceAuction
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

  pure auctionInfo

discoverSellerSignature :: AuctionInfoExtended -> Maybe VerificationKey -> Contract ByteArray
discoverSellerSignature (AuctionInfoExtended auctionInfo) bidderVk = do
  sellerSignature <- liftedE $ runExceptT $
    discoverSellerSignatureWithErrors
      ( wrap
          { auctionCs: auctionInfo.auctionId
          , sellerAddress: (unwrap auctionInfo.auctionTerms).sellerAddress
          , bidderVk
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
