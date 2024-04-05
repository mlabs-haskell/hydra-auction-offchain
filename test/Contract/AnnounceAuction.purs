module Test.Contract.AnnounceAuction
  ( AuctionTermsMutator
  , announceAuction
  , announceAuctionFix
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
import Data.Newtype (wrap)
import Data.Time.Duration (Seconds(Seconds))
import HydraAuctionOffchain.Contract
  ( AnnounceAuctionContractResult
  , mkAnnounceAuctionContractWithErrors
  )
import HydraAuctionOffchain.Contract.MintingPolicies (mkAlwaysMintsPolicy)
import HydraAuctionOffchain.Contract.Types
  ( AuctionTermsInput
  , emptySubmitTxData
  , submitTxReturningContractResult
  )
import HydraAuctionOffchain.Helpers (mkPosixTimeUnsafe)
import Mote (group, test)
import Test.Contract.Fixtures (auctionLotTokenNameFixture, auctionTermsInputFixture)
import Test.Helpers (defDistribution)

suite :: TestPlanM ContractTest Unit
suite =
  group "announce-auction" do
    test "seller announces an auction" do
      withWallets defDistribution \seller ->
        withKeyWallet seller do
          void announceAuction

type AuctionTermsMutator = AuctionTermsInput -> AuctionTermsInput

announceAuction :: Contract AnnounceAuctionContractResult
announceAuction = announceAuctionFix identity

announceAuctionFix :: AuctionTermsMutator -> Contract AnnounceAuctionContractResult
announceAuctionFix fixAuctionTerms = do
  auctionLotValue <- mintAuctionLot
  biddingStart <-
    currentTime <#>
      add (mkPosixTimeUnsafe $ Seconds 5.0)
  let
    auctionTerms =
      fixAuctionTerms $
        auctionTermsInputFixture auctionLotValue biddingStart
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
