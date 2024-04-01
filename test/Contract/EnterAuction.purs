module Test.Contract.EnterAuction
  ( enterAuction
  , genInvalidBidderDeposit
  , genValidBidderDeposit
  , suite
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftedE)
import Contract.Test (ContractTest, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Control.Monad.Except (runExceptT)
import HydraAuctionOffchain.Contract (mkEnterAuctionContractWithErrors)
import HydraAuctionOffchain.Contract.Types (AuctionInfoExtended, ContractResult)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, toInt) as BigInt
import Mote (group, test)
import Test.Contract.AnnounceAuction (announceAuction)
import Test.Helpers (defDistribution)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSampleOne)

suite :: TestPlanM ContractTest Unit
suite =
  group "enter-auction" do
    test "bidder enters the auction with valid deposit" do
      withWallets (defDistribution /\ defDistribution) \(seller /\ bidder) -> do
        { txHash, auctionInfo } <- withKeyWallet seller $ announceAuction Nothing
        awaitTxConfirmed txHash
        depositAmount <- liftEffect $ randomSampleOne $ genValidBidderDeposit auctionInfo
        withKeyWallet bidder do
          void $ enterAuction auctionInfo depositAmount

    test "bidder enters the auction with invalid deposit" do
      withWallets (defDistribution /\ defDistribution) \(seller /\ bidder) -> do
        { txHash, auctionInfo } <- withKeyWallet seller $ announceAuction Nothing
        awaitTxConfirmed txHash
        depositAmount <- liftEffect $ randomSampleOne $ genInvalidBidderDeposit auctionInfo
        withKeyWallet bidder do
          void $ enterAuction auctionInfo depositAmount

enterAuction :: AuctionInfoExtended -> BigInt -> Contract ContractResult
enterAuction auctionInfo depositAmount =
  liftedE $ runExceptT $
    mkEnterAuctionContractWithErrors
      ( wrap
          { auctionInfo
          , depositAmount: Just depositAmount
          }
      )

genValidBidderDeposit :: AuctionInfoExtended -> Gen BigInt
genValidBidderDeposit auctionInfo = do
  let minDepositAmount = (unwrap (unwrap auctionInfo).auctionTerms).minDepositAmount
  delta <- chooseInt zero 10_000_000
  pure $ minDepositAmount + BigInt.fromInt delta

genInvalidBidderDeposit :: AuctionInfoExtended -> Gen BigInt
genInvalidBidderDeposit auctionInfo = do
  let
    minDepositAmount = (unwrap (unwrap auctionInfo).auctionTerms).minDepositAmount
    upperBound = fromMaybe top $ BigInt.toInt minDepositAmount
  BigInt.fromInt <$> chooseInt zero (upperBound - one)
