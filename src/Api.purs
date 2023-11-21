module HydraAuctionOffchain.Api
  ( announceAuction
  ) where

import Prelude

import Contract.Monad (runContract)
import Contract.Transaction (TransactionHash)
import Control.Promise (Promise)
import Data.Argonaut (Json)
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (Aff)
import HydraAuctionOffchain.Codec (liftAff2)
import HydraAuctionOffchain.Config (mkContractParams)
import HydraAuctionOffchain.Contract (AnnounceAuctionContractParams)
import HydraAuctionOffchain.Contract (announceAuctionContract) as Contract
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
  announceAuctionContract walletApp =
    runContract (mkContractParams walletApp) <<< Contract.announceAuctionContract
