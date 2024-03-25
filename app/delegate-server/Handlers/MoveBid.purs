module DelegateServer.Handlers.MoveBid
  ( moveBidHandler
  ) where

import Prelude

import Contract.Transaction (TransactionHash)
import Control.Monad.Except (runExceptT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either(Left, Right))
import Data.Tuple.Nested ((/\))
import DelegateServer.Contract.Commit (CommitStandingBidError, commitStandingBid)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.Lib.ServerResponse (respBadRequest, respCreated)
import DelegateServer.State (class AppInit, readAppState, setCommitStatus)
import DelegateServer.Types.CommitStatus (CommitStatus(ShouldCommitStandingBid))
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus(HeadStatus_Idle, HeadStatus_Initializing)
  , printHeadStatus
  )
import Effect.Class (liftEffect)
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Codec (transactionHashCodec)
import HydraAuctionOffchain.Contract.Types (StandingBidState, standingBidStateCodec)
import Type.Proxy (Proxy(Proxy))

data MoveBidSuccess
  = MoveBidSuccess_SentInitHeadRequest
  | MoveBidSuccess_CommittedStandingBid StandingBidState TransactionHash

instance EncodeJson MoveBidSuccess where
  encodeJson = case _ of
    MoveBidSuccess_SentInitHeadRequest ->
      encodeJson
        { success: "Sent request to initialize a new Hydra Head."
        }
    MoveBidSuccess_CommittedStandingBid standingBid txHash ->
      encodeJson
        { success: "Committed standing bid."
        , standingBid: CA.encode standingBidStateCodec standingBid
        , txHash: CA.encode transactionHashCodec txHash
        }

data MoveBidError
  = MoveBidError_InvalidHeadStatus HydraHeadStatus
  | MoveBidError_CouldNotCommitStandingBid CommitStandingBidError

instance EncodeJson MoveBidError where
  encodeJson = case _ of
    MoveBidError_InvalidHeadStatus status ->
      encodeJson
        { error: "Invalid head status: " <> printHeadStatus status <> "."
        }
    MoveBidError_CouldNotCommitStandingBid err ->
      encodeJson
        { error: "Could not commit standing bid, error: " <> show err <> "."
        }

moveBidHandler :: forall m. AppInit m => HydraNodeApiWebSocket -> m HTTPure.Response
moveBidHandler ws = do
  setCommitStatus ShouldCommitStandingBid
  headStatus <- readAppState (Proxy :: _ "headStatus")
  case headStatus of
    HeadStatus_Idle -> do
      liftEffect ws.initHead
      respCreated MoveBidSuccess_SentInitHeadRequest
    HeadStatus_Initializing ->
      runExceptT commitStandingBid >>= case _ of
        Left err ->
          respBadRequest $
            MoveBidError_CouldNotCommitStandingBid err
        Right (standingBid /\ txHash) ->
          respCreated $
            MoveBidSuccess_CommittedStandingBid standingBid txHash
    _ ->
      respBadRequest $
        MoveBidError_InvalidHeadStatus headStatus
