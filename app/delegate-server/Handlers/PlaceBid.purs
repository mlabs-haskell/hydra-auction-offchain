module DelegateServer.Handlers.PlaceBid
  ( placeBidHandler
  ) where

import Prelude

import Data.Either (Either(Left, Right))
import DelegateServer.Contract.PlaceBid (placeBidL2)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (AppM)
import HTTPure (Response, badRequest, created) as HTTPure
import HydraAuctionOffchain.Contract.Types (bidTermsCodec)
import HydraAuctionOffchain.Lib.Json (caDecodeString)

placeBidHandler :: HydraNodeApiWebSocket -> String -> AppM HTTPure.Response
placeBidHandler ws bodyStr =
  case caDecodeString bidTermsCodec bodyStr of
    Left err ->
      HTTPure.badRequest err
    Right bidTerms -> do
      placeBidL2 ws bidTerms
      HTTPure.created
