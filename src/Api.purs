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
  , getWalletVk
  , mintTokenUsingAlwaysMints
  , moveBidL2
  , placeBid
  , placeBidL2
  , queryAuctions
  , queryStandingBidState
  , startBidding
  ) where

import Prelude

import Cardano.Types (NetworkId)
import Contract.Address (getNetworkId)
import Contract.Monad (Contract, runContract)
import Contract.Transaction (TransactionHash)
import Contract.Transaction (awaitTxConfirmed) as Contract
import Control.Promise (Promise, fromAff)
import Data.Argonaut (Json)
import Effect (Effect)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Contract
  ( announceAuctionContract
  , authorizeBiddersContract
  , claimAuctionLotBidderContract
  , claimAuctionLotSellerContract
  , discoverBidders
  , discoverSellerSignature
  , enterAuctionContract
  , getWalletVk
  , mintTokenUsingAlwaysMints
  , moveBidContract
  , placeBidContract
  , queryAuctions
  , queryStandingBidState
  , sendBidContract
  , startBiddingContract
  ) as Contract
import HydraAuctionOffchain.Contract.Types (AuctionInfo, ContractOutput(ContractOutputResult))
import HydraAuctionOffchain.Lib.Codec (class HasJson, fromJs, toJs)
import HydraAuctionOffchain.Types.ContractConfig (mkContractParams)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)

contractGeneric
  :: forall a b
   . HasJson a NetworkId
  => HasJson b NetworkId
  => (a -> Contract b)
  -> Json
  -> Json
  -> Effect (Promise Json)
contractGeneric contract contractConfig params = fromAff do
  contractParams <- mkContractParams $ fromJs unit contractConfig
  runContract contractParams do
    network <- getNetworkId
    toJs network <$> contract (fromJs network params)

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

moveBidL2 :: Json -> Json -> Effect (Promise Json)
moveBidL2 = contractGeneric Contract.moveBidContract

placeBidL2 :: Json -> Json -> Effect (Promise Json)
placeBidL2 = contractGeneric Contract.sendBidContract

queryAuctions :: Json -> Json -> Effect (Promise Json)
queryAuctions = contractGeneric Contract.queryAuctions

queryStandingBidState :: Json -> Json -> Effect (Promise Json)
queryStandingBidState = contractGeneric Contract.queryStandingBidState

startBidding :: Json -> Json -> Effect (Promise Json)
startBidding = contractGeneric Contract.startBiddingContract

claimAuctionLotBidder :: Json -> Json -> Effect (Promise Json)
claimAuctionLotBidder = contractGeneric Contract.claimAuctionLotBidderContract

claimAuctionLotSeller :: Json -> Json -> Effect (Promise Json)
claimAuctionLotSeller = contractGeneric Contract.claimAuctionLotSellerContract

claimDepositLoser :: Json -> Json -> Effect (Promise Json)
claimDepositLoser = contractGeneric contractStub

cleanupAuction :: Json -> Json -> Effect (Promise Json)
cleanupAuction = contractGeneric contractStub

contractStub :: AuctionInfo -> Contract (ContractOutput TransactionHash)
contractStub _ = do
  txHash <- liftEffect $ randomSampleOne arbitrary
  pure $ ContractOutputResult txHash

----------------------------------------------------------------------
-- Helpers

getWalletVk :: Json -> Effect (Promise Json)
getWalletVk contractConfig = fromAff do
  contractParams <- mkContractParams $ fromJs unit contractConfig
  toJs unit <$> runContract contractParams Contract.getWalletVk

awaitTxConfirmed :: Json -> Json -> Effect (Promise Unit)
awaitTxConfirmed contractConfig txHash = fromAff do
  contractParams <- mkContractParams $ fromJs unit contractConfig
  runContract contractParams $ Contract.awaitTxConfirmed $ fromJs unit txHash

mintTokenUsingAlwaysMints :: Json -> Json -> Json -> Effect (Promise Json)
mintTokenUsingAlwaysMints contractConfig tokenName quantity = fromAff do
  contractParams <- mkContractParams $ fromJs unit contractConfig
  toJs unit <$> runContract contractParams
    (Contract.mintTokenUsingAlwaysMints (fromJs unit tokenName) (fromJs unit quantity))
