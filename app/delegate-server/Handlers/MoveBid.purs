module DelegateServer.Handlers.MoveBid
  ( MoveBidError
      ( MoveBidError_InvalidHeadStatus
      , MoveBidError_CouldNotCommitStandingBid
      )
  , MoveBidResponse
  , MoveBidSuccess
      ( MoveBidSuccess_SentInitHeadRequest
      , MoveBidSuccess_CommittedStandingBid
      )
  , moveBidHandler
  , moveBidHandlerImpl
  ) where

import Prelude

import Contract.Transaction (TransactionHash)
import Control.Monad.Except (runExceptT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import DelegateServer.Contract.Commit (CommitStandingBidError, commitStandingBid)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (class AppInit, readAppState, setCommitStatus)
import DelegateServer.Types.CommitStatus (CommitStatus(ShouldCommitStandingBid))
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus(HeadStatus_Idle, HeadStatus_Initializing)
  , printHeadStatus
  )
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  , respCreatedOrBadRequest
  )
import Effect.Class (liftEffect)
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Codec (transactionHashCodec)
import HydraAuctionOffchain.Contract.Types (StandingBidState, standingBidStateCodec)
import Type.Proxy (Proxy(Proxy))

type MoveBidResponse = ServerResponse MoveBidSuccess MoveBidError

data MoveBidSuccess
  = MoveBidSuccess_SentInitHeadRequest
  | MoveBidSuccess_CommittedStandingBid StandingBidState TransactionHash

derive instance Generic MoveBidSuccess _
derive instance Eq MoveBidSuccess

instance Show MoveBidSuccess where
  show = genericShow

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

derive instance Generic MoveBidError _
derive instance Eq MoveBidError

instance Show MoveBidError where
  show = genericShow

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
moveBidHandler = respCreatedOrBadRequest <=< moveBidHandlerImpl

moveBidHandlerImpl :: forall m. AppInit m => HydraNodeApiWebSocket -> m MoveBidResponse
moveBidHandlerImpl ws = do
  setCommitStatus ShouldCommitStandingBid
  headStatus <- readAppState (Proxy :: _ "headStatus")
  case headStatus of
    HeadStatus_Idle -> do
      liftEffect ws.initHead
      pure $ ServerResponseSuccess MoveBidSuccess_SentInitHeadRequest
    HeadStatus_Initializing ->
      runExceptT commitStandingBid <#> case _ of
        Left err ->
          ServerResponseError $
            MoveBidError_CouldNotCommitStandingBid err
        Right (standingBid /\ txHash) ->
          ServerResponseSuccess $
            MoveBidSuccess_CommittedStandingBid standingBid txHash
    _ ->
      pure $ ServerResponseError $
        MoveBidError_InvalidHeadStatus headStatus
