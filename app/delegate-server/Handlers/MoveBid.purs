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
import Data.Argonaut (class EncodeJson)
import Data.Codec.Argonaut (JsonCodec, encode, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.Variant (inj, match) as Variant
import DelegateServer.Contract.Commit
  ( CommitStandingBidError
  , commitStandingBid
  , commitStandingBidErrorCodec
  )
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (class AppInit, readAppState, setCommitStatus)
import DelegateServer.Types.CommitStatus (CommitStatus(ShouldCommitStandingBid))
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus(HeadStatus_Idle, HeadStatus_Initializing)
  , headStatusCodec
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
            MoveBidSuccess_CommittedStandingBid { standingBid, txHash }
    _ ->
      pure $ ServerResponseError $
        MoveBidError_InvalidHeadStatus headStatus

-- MoveBidSuccess ----------------------------------------------------

data MoveBidSuccess
  = MoveBidSuccess_SentInitHeadRequest
  | MoveBidSuccess_CommittedStandingBid
      { standingBid :: StandingBidState
      , txHash :: TransactionHash
      }

derive instance Generic MoveBidSuccess _
derive instance Eq MoveBidSuccess

instance Show MoveBidSuccess where
  show = genericShow

instance EncodeJson MoveBidSuccess where
  encodeJson = CA.encode moveBidSuccessCodec

moveBidSuccessCodec :: CA.JsonCodec MoveBidSuccess
moveBidSuccessCodec =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "SentInitHeadRequest": Left unit
        , "CommittedStandingBid":
            Right $ CA.object "MoveBidSuccess_CommittedStandingBid" $ CAR.record
              { standingBid: standingBidStateCodec
              , txHash: transactionHashCodec
              }
        }
    )
  where
  toVariant = case _ of
    MoveBidSuccess_SentInitHeadRequest ->
      Variant.inj (Proxy :: _ "SentInitHeadRequest") unit
    MoveBidSuccess_CommittedStandingBid x ->
      Variant.inj (Proxy :: _ "CommittedStandingBid") x

  fromVariant = Variant.match
    { "SentInitHeadRequest": const MoveBidSuccess_SentInitHeadRequest
    , "CommittedStandingBid": MoveBidSuccess_CommittedStandingBid
    }

-- MoveBidError ------------------------------------------------------

data MoveBidError
  = MoveBidError_InvalidHeadStatus HydraHeadStatus
  | MoveBidError_CouldNotCommitStandingBid CommitStandingBidError

derive instance Generic MoveBidError _
derive instance Eq MoveBidError

instance Show MoveBidError where
  show = genericShow

instance EncodeJson MoveBidError where
  encodeJson = CA.encode moveBidErrorCodec

moveBidErrorCodec :: CA.JsonCodec MoveBidError
moveBidErrorCodec =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "InvalidHeadStatus": Right headStatusCodec
        , "CouldNotCommitStandingBid": Right commitStandingBidErrorCodec
        }
    )
  where
  toVariant = case _ of
    MoveBidError_InvalidHeadStatus x ->
      Variant.inj (Proxy :: _ "InvalidHeadStatus") x
    MoveBidError_CouldNotCommitStandingBid x ->
      Variant.inj (Proxy :: _ "CouldNotCommitStandingBid") x

  fromVariant = Variant.match
    { "InvalidHeadStatus": MoveBidError_InvalidHeadStatus
    , "CouldNotCommitStandingBid": MoveBidError_CouldNotCommitStandingBid
    }
