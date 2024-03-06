module DelegateServer.ClientServer.Handlers.PlaceBid
  ( placeBidHandler
  ) where

import Data.Either (Either(Left, Right))
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (AppM)
import HTTPure (Response, badRequest, notFound) as HTTPure
import HydraAuctionOffchain.Contract.Types (bidTermsCodec)
import HydraAuctionOffchain.Lib.Json (caDecodeString)

placeBidHandler :: HydraNodeApiWebSocket -> String -> AppM HTTPure.Response
placeBidHandler _ws bodyStr =
  case caDecodeString bidTermsCodec bodyStr of
    Left err ->
      HTTPure.badRequest err
    Right _bidTerms -> do
      -- placeBid bidTerms
      HTTPure.notFound
