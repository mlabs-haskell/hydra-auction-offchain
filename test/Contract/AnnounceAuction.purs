module Test.Contract.AnnounceAuction
  ( AuctionTermsMutator
  , announceAuction
  , announceAuctionFix
  , suite
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.CurrencySymbol (fromScriptHash)
import Cardano.Plutus.Types.Value (Value) as Plutus
import Cardano.Plutus.Types.Value (singleton) as Plutus.Value
import Cardano.Types (Mint, ScriptHash)
import Cardano.Types.Int (Int) as Cardano
import Cardano.Types.Int (one, toBigInt) as Cardano.Int
import Cardano.Types.Mint (singleton) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Chain (currentTime)
import Contract.Monad (Contract, liftedE)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (plutusMintingPolicy) as Lookups
import Contract.Test (ContractTest, withKeyWallet, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (awaitTxConfirmed)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustMintValue) as Constraints
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
      , delegateInfo: Nothing
      , additionalAuctionLotOrefs: mempty
      }
  liftedE $ runExceptT $ mkAnnounceAuctionContractWithErrors params

mintAuctionLot :: Contract Plutus.Value
mintAuctionLot = do
  alwaysMintsPolicy <- mkAlwaysMintsPolicy
  let
    cs :: ScriptHash
    cs = PlutusScript.hash alwaysMintsPolicy

    mintQuantity :: Cardano.Int
    mintQuantity = Cardano.Int.one

    auctionLotValue :: Mint
    auctionLotValue = Mint.singleton cs auctionLotTokenNameFixture mintQuantity

    auctionLotValuePlutus :: Plutus.Value
    auctionLotValuePlutus = Plutus.Value.singleton
      (fromScriptHash cs)
      (wrap auctionLotTokenNameFixture)
      (Cardano.Int.toBigInt mintQuantity)

    constraints :: TxConstraints
    constraints = Constraints.mustMintValue auctionLotValue

    lookups :: ScriptLookups
    lookups = Lookups.plutusMintingPolicy alwaysMintsPolicy

  { txHash } <- submitTxReturningContractResult {} $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    }
  awaitTxConfirmed txHash
  pure auctionLotValuePlutus
