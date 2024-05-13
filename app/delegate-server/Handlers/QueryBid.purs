module DelegateServer.Handlers.QueryBid
  ( queryBidHandler
  ) where

import Prelude

import Data.Maybe (maybe)
import Data.Tuple (snd)
import DelegateServer.Contract.StandingBid (queryStandingBidL2)
import DelegateServer.State (class AppOpen)
import HTTPure (Response, notFound, ok) as HTTPure
import HydraAuctionOffchain.Contract.Types (standingBidStateCodec)
import HydraAuctionOffchain.Lib.Json (caEncodeString)

queryBidHandler :: forall m. AppOpen m => m HTTPure.Response
queryBidHandler =
  queryStandingBidL2 >>=
    maybe HTTPure.notFound
      (HTTPure.ok <<< caEncodeString standingBidStateCodec <<< snd)
