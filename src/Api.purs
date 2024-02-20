module HydraAuctionOffchain.Api
  ( announceAuction
  , authorizeBidders
  , awaitTxConfirmed
  , claimAuctionLotBidder
  , claimAuctionLotSeller
  , claimDepositLoser
  , cleanupAuction
  , discoverBidders
  , discoverSellerSignature
  , enterAuction
  , mintTokenUsingAlwaysMints
  , placeBid
  , queryAuctions
  , queryStandingBidState
  , startBidding
  ) where

import Prelude

import Contract.Monad (Contract, runContract)
import Contract.Transaction (TransactionHash)
import Contract.Transaction (awaitTxConfirmed) as Contract
import Control.Promise (Promise, fromAff)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(Just))
import Effect (Effect)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Codec (class HasJson, fromJs, toJs)
import HydraAuctionOffchain.Config (mkContractParams)
import HydraAuctionOffchain.Contract
  ( announceAuctionContract
  , authorizeBiddersContract
  , claimAuctionLotBidderContract
  , discoverBidders
  , discoverSellerSignature
  , enterAuctionContract
  , mintTokenUsingAlwaysMints
  , placeBidContract
  , queryAuctions
  , queryStandingBidState
  , startBiddingContract
  ) as Contract
import HydraAuctionOffchain.Contract.Types (AuctionInfo, ContractOutput(ContractOutputResult))
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

contractGeneric
  :: forall a b
   . HasJson a
  => HasJson b
  => (a -> Contract b)
  -> Json
  -> Json
  -> Effect (Promise Json)
contractGeneric contract walletApp params = fromAff do
  contractParams <- mkContractParams $ fromJs walletApp
  toJs <$> runContract contractParams (contract $ fromJs params)

----------------------------------------------------------------------
-- Auctions

announceAuction :: Json -> Json -> Effect (Promise Json)
announceAuction = contractGeneric Contract.announceAuctionContract

authorizeBidders :: Json -> Json -> Effect (Promise Json)
authorizeBidders = contractGeneric Contract.authorizeBiddersContract

discoverBidders :: Json -> Json -> Effect (Promise Json)
discoverBidders = contractGeneric Contract.discoverBidders

discoverSellerSignature :: Json -> Json -> Effect (Promise Json)
discoverSellerSignature = contractGeneric Contract.discoverSellerSignature

enterAuction :: Json -> Json -> Effect (Promise Json)
enterAuction = contractGeneric Contract.enterAuctionContract

placeBid :: Json -> Json -> Effect (Promise Json)
placeBid = contractGeneric Contract.placeBidContract

queryAuctions :: Json -> Json -> Effect (Promise Json)
queryAuctions = contractGeneric Contract.queryAuctions

queryStandingBidState :: Json -> Json -> Effect (Promise Json)
queryStandingBidState = contractGeneric Contract.queryStandingBidState

startBidding :: Json -> Json -> Effect (Promise Json)
startBidding = contractGeneric Contract.startBiddingContract

claimAuctionLotBidder :: Json -> Json -> Effect (Promise Json)
claimAuctionLotBidder = contractGeneric Contract.claimAuctionLotBidderContract

claimDepositLoser :: Json -> Json -> Effect (Promise Json)
claimDepositLoser = contractGeneric contractStub

claimAuctionLotSeller :: Json -> Json -> Effect (Promise Json)
claimAuctionLotSeller = contractGeneric contractStub

cleanupAuction :: Json -> Json -> Effect (Promise Json)
cleanupAuction = contractGeneric contractStub

contractStub :: AuctionInfo -> Contract (ContractOutput TransactionHash)
contractStub _ = do
  txHash <- liftEffect $ randomSampleOne arbitrary
  pure $ ContractOutputResult txHash

----------------------------------------------------------------------
-- Helpers

awaitTxConfirmed :: Json -> Json -> Effect (Promise Unit)
awaitTxConfirmed walletApp txHash = fromAff do
  contractParams <- mkContractParams $ fromJs walletApp
  runContract contractParams $ Contract.awaitTxConfirmed $ fromJs txHash

mintTokenUsingAlwaysMints :: Json -> Json -> Json -> Effect (Promise Json)
mintTokenUsingAlwaysMints walletApp tokenName quantity = fromAff do
  contractParams <- mkContractParams $ Just $ fromJs walletApp
  toJs <$> runContract contractParams
    (Contract.mintTokenUsingAlwaysMints (fromJs tokenName) (fromJs quantity))
