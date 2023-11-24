module HydraAuctionOffchain.Api
  ( announceAuction
  , queryAuctions
  ) where

import Prelude

import Contract.Monad (runContract)
import Contract.Transaction (TransactionHash)
import Control.Promise (Promise, fromAff)
import Data.Argonaut (Json)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Aff (Aff)
import HydraAuctionOffchain.Codec (liftAff2, toJs)
import HydraAuctionOffchain.Config (mkContractParams)
import HydraAuctionOffchain.Contract (AnnounceAuctionContractParams)
import HydraAuctionOffchain.Contract (announceAuctionContract, queryAuctions) as Contract
import HydraAuctionOffchain.Contract.Types (ContractOutput)
import HydraAuctionOffchain.WalletApp (WalletApp)

announceAuction :: Json -> Json -> Effect (Promise Json)
announceAuction walletApp params =
  liftAff2 announceAuctionContract walletApp params
  where
  announceAuctionContract
    :: WalletApp
    -> AnnounceAuctionContractParams
    -> Aff (ContractOutput TransactionHash)
  announceAuctionContract walletApp' =
    runContract (mkContractParams $ Just walletApp') <<< Contract.announceAuctionContract

queryAuctions :: Effect (Promise Json)
queryAuctions = fromAff $ toJs <$> runContract (mkContractParams Nothing)
  Contract.queryAuctions
