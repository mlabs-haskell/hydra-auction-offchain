module HydraAuctionOffchain.Api
  ( announceAuction
  , authorizeBidders
  , awaitTxConfirmed
  , discoverBidders
  , enterAuction
  , mintTokenUsingAlwaysMints
  , startBidding
  , queryAuctions
  ) where

import Prelude

import Contract.Monad (runContract)
import Contract.Transaction (awaitTxConfirmed) as Contract
import Control.Promise (Promise, fromAff)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(Just))
import Effect (Effect)
import HydraAuctionOffchain.Codec (fromJs, toJs)
import HydraAuctionOffchain.Config (mkContractParams)
import HydraAuctionOffchain.Contract
  ( announceAuctionContract
  , authorizeBiddersContract
  , discoverBidders
  , enterAuctionContract
  , mintTokenUsingAlwaysMints
  , startBiddingContract
  , queryAuctions
  ) as Contract

----------------------------------------------------------------------
-- Auctions

announceAuction :: Json -> Json -> Effect (Promise Json)
announceAuction walletApp params = fromAff do
  contractParams <- mkContractParams $ Just $ fromJs walletApp
  toJs <$> runContract contractParams (Contract.announceAuctionContract $ fromJs params)

enterAuction :: Json -> Json -> Effect (Promise Json)
enterAuction walletApp params = fromAff do
  contractParams <- mkContractParams $ Just $ fromJs walletApp
  toJs <$> runContract contractParams (Contract.enterAuctionContract $ fromJs params)

discoverBidders :: Json -> Json -> Effect (Promise Json)
discoverBidders walletApp auctionInfo = fromAff do
  contractParams <- mkContractParams $ fromJs walletApp
  toJs <$> runContract contractParams (Contract.discoverBidders $ fromJs auctionInfo)

authorizeBidders :: Json -> Json -> Effect (Promise Json)
authorizeBidders walletApp params = fromAff do
  contractParams <- mkContractParams $ Just $ fromJs walletApp
  toJs <$> runContract contractParams (Contract.authorizeBiddersContract $ fromJs params)

queryAuctions :: Json -> Effect (Promise Json)
queryAuctions walletApp = fromAff do
  contractParams <- mkContractParams $ fromJs walletApp
  toJs <$> runContract contractParams Contract.queryAuctions

startBidding :: Json -> Json -> Effect (Promise Json)
startBidding walletApp params = fromAff do
  contractParams <- mkContractParams $ Just $ fromJs walletApp
  toJs <$> runContract contractParams (Contract.startBiddingContract $ fromJs params)

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
