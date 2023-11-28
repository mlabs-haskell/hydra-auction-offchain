module HydraAuctionOffchain.Api
  ( announceAuction
  , queryAuctions
  ) where

import Prelude

import Contract.Monad (runContract)
import Control.Promise (Promise, fromAff)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(Just))
import Effect (Effect)
import HydraAuctionOffchain.Codec (fromJs, toJs)
import HydraAuctionOffchain.Config (mkContractParams)
import HydraAuctionOffchain.Contract (announceAuctionContract, queryAuctions) as Contract

announceAuction :: Json -> Json -> Effect (Promise Json)
announceAuction walletApp params = fromAff do
  contractParams <- mkContractParams $ Just $ fromJs walletApp
  toJs <$> runContract contractParams (Contract.announceAuctionContract $ fromJs params)

queryAuctions :: Json -> Effect (Promise Json)
queryAuctions walletApp = fromAff do
  contractParams <- mkContractParams $ fromJs walletApp
  toJs <$> runContract contractParams Contract.queryAuctions
