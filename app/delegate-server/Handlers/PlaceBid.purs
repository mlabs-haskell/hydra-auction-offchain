module DelegateServer.Handlers.PlaceBid
  ( placeBidHandler
  ) where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either(Left, Right))
import DelegateServer.Contract.PlaceBid (PlaceBidL2ContractError, placeBidL2)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.Lib.ServerResponse (respBadRequest, respCreated)
import DelegateServer.State (class AppOpen)
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Contract.Types (bidTermsCodec, contractErrorCodec, toContractError)
import HydraAuctionOffchain.Lib.Json (caDecodeString)

data PlaceBidSuccess = PlaceBidSuccess_SubmittedTransaction

instance EncodeJson PlaceBidSuccess where
  encodeJson = case _ of
    PlaceBidSuccess_SubmittedTransaction ->
      encodeJson
        { success: "Submitted PlaceBid L2 transaction."
        }

data PlaceBidError
  = PlaceBidError_CouldNotDecodeBidTerms String
  | PlaceBidError_ContractError PlaceBidL2ContractError

instance EncodeJson PlaceBidError where
  encodeJson = case _ of
    PlaceBidError_CouldNotDecodeBidTerms err ->
      encodeJson
        { error: "Could not decode provided payload as BidTerms, error: " <> err
        }
    PlaceBidError_ContractError err ->
      encodeJson
        { error: "Could not place bid on L2 due to contract validation error."
        , body: CA.encode contractErrorCodec $ toContractError err
        }

placeBidHandler :: forall m. AppOpen m => HydraNodeApiWebSocket -> String -> m HTTPure.Response
placeBidHandler ws bodyStr =
  case caDecodeString bidTermsCodec bodyStr of
    Left decodeErr ->
      respBadRequest $
        PlaceBidError_CouldNotDecodeBidTerms decodeErr
    Right bidTerms -> do
      runExceptT (placeBidL2 ws bidTerms) >>= case _ of
        Left contractErr ->
          respBadRequest $
            PlaceBidError_ContractError contractErr
        Right _ ->
          respCreated PlaceBidSuccess_SubmittedTransaction
