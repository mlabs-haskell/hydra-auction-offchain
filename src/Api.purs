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
  , queryDelegateGroups
  , queryStandingBidState
  , registerDelegateGroup
  , startBidding
  ) where

import Prelude

import Aeson (Aeson)
import Cardano.Types (NetworkId)
import Contract.Address (getNetworkId)
import Contract.Monad (Contract, runContract)
import Contract.Transaction (TransactionHash)
import Contract.Transaction (awaitTxConfirmed) as Contract
import Control.Promise (Promise, fromAff)
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
  , queryDelegateGroups
  , queryStandingBidState
  , registerDelegateGroupContract
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
  -> Aeson
  -> Aeson
  -> Effect (Promise Aeson)
contractGeneric contract contractConfig params = fromAff do
  let contractParams = mkContractParams $ fromJs unit contractConfig
  runContract contractParams do
    network <- getNetworkId
    toJs network <$> contract (fromJs network params)

----------------------------------------------------------------------
-- Delegate groups

registerDelegateGroup :: Aeson -> Aeson -> Effect (Promise Aeson)
registerDelegateGroup = contractGeneric Contract.registerDelegateGroupContract

queryDelegateGroups :: Aeson -> Effect (Promise Aeson)
queryDelegateGroups contractConfig = fromAff do
  let contractParams = mkContractParams $ fromJs unit contractConfig
  toJs unit <$> runContract contractParams Contract.queryDelegateGroups

----------------------------------------------------------------------
-- Auctions

announceAuction :: Aeson -> Aeson -> Effect (Promise Aeson)
announceAuction = contractGeneric Contract.announceAuctionContract

authorizeBidders :: Aeson -> Aeson -> Effect (Promise Aeson)
authorizeBidders = contractGeneric Contract.authorizeBiddersContract

discoverBidders :: Aeson -> Aeson -> Effect (Promise Aeson)
discoverBidders = contractGeneric Contract.discoverBidders

discoverSellerSignature :: Aeson -> Aeson -> Effect (Promise Aeson)
discoverSellerSignature = contractGeneric Contract.discoverSellerSignature

enterAuction :: Aeson -> Aeson -> Effect (Promise Aeson)
enterAuction = contractGeneric Contract.enterAuctionContract

placeBid :: Aeson -> Aeson -> Effect (Promise Aeson)
placeBid = contractGeneric Contract.placeBidContract

moveBidL2 :: Aeson -> Aeson -> Effect (Promise Aeson)
moveBidL2 = contractGeneric Contract.moveBidContract

placeBidL2 :: Aeson -> Aeson -> Effect (Promise Aeson)
placeBidL2 = contractGeneric Contract.sendBidContract

queryAuctions :: Aeson -> Aeson -> Effect (Promise Aeson)
queryAuctions = contractGeneric Contract.queryAuctions

queryStandingBidState :: Aeson -> Aeson -> Effect (Promise Aeson)
queryStandingBidState = contractGeneric Contract.queryStandingBidState

startBidding :: Aeson -> Aeson -> Effect (Promise Aeson)
startBidding = contractGeneric Contract.startBiddingContract

claimAuctionLotBidder :: Aeson -> Aeson -> Effect (Promise Aeson)
claimAuctionLotBidder = contractGeneric Contract.claimAuctionLotBidderContract

claimAuctionLotSeller :: Aeson -> Aeson -> Effect (Promise Aeson)
claimAuctionLotSeller = contractGeneric Contract.claimAuctionLotSellerContract

claimDepositLoser :: Aeson -> Aeson -> Effect (Promise Aeson)
claimDepositLoser = contractGeneric contractStub

cleanupAuction :: Aeson -> Aeson -> Effect (Promise Aeson)
cleanupAuction = contractGeneric contractStub

contractStub :: AuctionInfo -> Contract (ContractOutput TransactionHash)
contractStub _ = do
  txHash <- liftEffect $ randomSampleOne arbitrary
  pure $ ContractOutputResult txHash

----------------------------------------------------------------------
-- Helpers

getWalletVk :: Aeson -> Effect (Promise Aeson)
getWalletVk contractConfig = fromAff do
  let contractParams = mkContractParams $ fromJs unit contractConfig
  toJs unit <$> runContract contractParams Contract.getWalletVk

awaitTxConfirmed :: Aeson -> Aeson -> Effect (Promise Unit)
awaitTxConfirmed contractConfig txHash = fromAff do
  let contractParams = mkContractParams $ fromJs unit contractConfig
  runContract contractParams $ Contract.awaitTxConfirmed $ fromJs unit txHash

mintTokenUsingAlwaysMints :: Aeson -> Aeson -> Aeson -> Effect (Promise Aeson)
mintTokenUsingAlwaysMints contractConfig tokenName quantity = fromAff do
  let contractParams = mkContractParams $ fromJs unit contractConfig
  toJs unit <$> runContract contractParams
    (Contract.mintTokenUsingAlwaysMints (fromJs unit tokenName) (fromJs unit quantity))
