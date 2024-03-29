module Test.Contract.AnnounceAuction
  ( announceAuction
  , suite
  ) where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Monad (Contract, liftedE)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (mintingPolicy) as Lookups
import Contract.Test (ContractTest, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustMintValue) as Constraints
import Contract.Value (CurrencySymbol, Value)
import Contract.Value (scriptCurrencySymbol, singleton) as Value
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (untilJust)
import Data.Newtype (wrap)
import Data.Time.Duration (Seconds(Seconds), fromDuration)
import Effect.Aff (delay)
import HydraAuctionOffchain.Contract
  ( AnnounceAuctionContractResult
  , mkAnnounceAuctionContractWithErrors
  )
import HydraAuctionOffchain.Contract.MintingPolicies (mkAlwaysMintsPolicy)
import HydraAuctionOffchain.Contract.Types (emptySubmitTxData, submitTxReturningContractResult)
import HydraAuctionOffchain.Helpers (mkPosixTimeUnsafe)
import Mote (group, test)
import Test.Contract.Fixtures (auctionLotTokenNameFixture, auctionTermsInputFixture)
import Test.DelegateServer.Cluster (withWallets')
import Test.Helpers (defDistribution)
import Test.Spec.Assertions (shouldEqual)

suite :: TestPlanM ContractTest Unit
suite =
  group "announce-auction" do
    test "seller announces an auction" do
      withWallets defDistribution \seller ->
        withKeyWallet seller do
          void announceAuction

    test "delegates set announced auction as active auction" do
      withWallets' defDistribution
        \seller withDelegateServerCluster -> do
          { txHash, auctionInfo } <- withKeyWallet seller announceAuction
          awaitTxConfirmed txHash

          withDelegateServerCluster auctionInfo \appHandle ->
            shouldEqual auctionInfo =<<
              untilJust do
                delay $ fromDuration $ Seconds one
                appHandle.getActiveAuction

announceAuction :: Contract AnnounceAuctionContractResult
announceAuction = do
  auctionLotValue <- mintAuctionLot
  biddingStart <-
    currentTime <#>
      add (mkPosixTimeUnsafe $ Seconds 5.0)
  let
    auctionTerms = auctionTermsInputFixture auctionLotValue biddingStart
    params = wrap
      { auctionTerms
      , additionalAuctionLotOrefs: mempty
      }
  liftedE $ runExceptT $ mkAnnounceAuctionContractWithErrors params

mintAuctionLot :: Contract Value
mintAuctionLot = do
  alwaysMintsPolicy <- mkAlwaysMintsPolicy
  let
    cs :: CurrencySymbol
    cs = Value.scriptCurrencySymbol alwaysMintsPolicy

    auctionLotValue :: Value
    auctionLotValue = Value.singleton cs auctionLotTokenNameFixture one

    constraints :: TxConstraints
    constraints = Constraints.mustMintValue auctionLotValue

    lookups :: ScriptLookups
    lookups = Lookups.mintingPolicy alwaysMintsPolicy

  { txHash } <- submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }
  awaitTxConfirmed txHash
  pure auctionLotValue
