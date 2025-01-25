module DelegateServer.PeerDelegate.Http
  ( signCommitTxRequest
  ) where

import Prelude

import Cardano.Types (ScriptHash)
import Ctl.Internal.Helpers ((<</>>))
import Data.Array (singleton) as Array
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import DelegateServer.Handlers.SignCommitTx
  ( SignCommitTxRequestPayload
  , SignCommitTxResponse
  , signCommitTxRequestPayloadCodec
  , signCommitTxResponseCodec
  )
import Effect.Aff (Aff)
import HydraAuctionOffchain.Service.Common (ServiceError)
import HydraAuctionOffchain.Service.DelegateServer.Http
  ( handleResponse
  , mkAuctionCsHeader
  , postRequest
  )

signCommitTxRequest
  :: String
  -> ScriptHash
  -> SignCommitTxRequestPayload
  -> Aff (Either ServiceError SignCommitTxResponse)
signCommitTxRequest httpServer auctionCs reqBody =
  handleResponse signCommitTxResponseCodec <$>
    postRequest
      { url: httpServer <</>> "signCommitTx"
      , content: Just $ CA.encode signCommitTxRequestPayloadCodec reqBody
      , headers: Array.singleton $ mkAuctionCsHeader auctionCs
      }
