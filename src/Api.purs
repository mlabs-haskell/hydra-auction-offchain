module HydraAuctionOffchain.Api
  ( announceAuction
  ) where

import Prelude

import Contract.Monad (runContract)
import Control.Promise (Promise)
import Data.Argonaut (Json)
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import HydraAuctionOffchain.Codec (liftAff1)
import HydraAuctionOffchain.Contract (announceAuctionContract)

announceAuction :: Json -> Effect (Promise Json)
announceAuction params = liftAff1 (runContract undefined <<< announceAuctionContract) params
