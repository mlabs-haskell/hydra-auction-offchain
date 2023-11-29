module HydraAuctionOffchain.Api
  ( announceAuction
  , awaitTxConfirmed
  , mintTokenUsingAlwaysMints
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
  , mintTokenUsingAlwaysMints
  , queryAuctions
  ) as Contract

--------------------------------------------------------------------------------
-- Auctions
--------------------------------------------------------------------------------

announceAuction :: Json -> Json -> Effect (Promise Json)
announceAuction walletApp params = fromAff do
  contractParams <- mkContractParams $ Just $ fromJs walletApp
  toJs <$> runContract contractParams (Contract.announceAuctionContract $ fromJs params)

queryAuctions :: Json -> Effect (Promise Json)
queryAuctions walletApp = fromAff do
  contractParams <- mkContractParams $ fromJs walletApp
  toJs <$> runContract contractParams Contract.queryAuctions

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

awaitTxConfirmed :: Json -> Json -> Effect (Promise Unit)
awaitTxConfirmed walletApp txHash = fromAff do
  contractParams <- mkContractParams $ fromJs walletApp
  runContract contractParams $ Contract.awaitTxConfirmed $ fromJs txHash

mintTokenUsingAlwaysMints :: Json -> Json -> Json -> Effect (Promise Json)
mintTokenUsingAlwaysMints walletApp tokenName quantity = fromAff do
  contractParams <- mkContractParams $ Just $ fromJs walletApp
  toJs <$> runContract contractParams
    (Contract.mintTokenUsingAlwaysMints (fromJs tokenName) (fromJs quantity))
