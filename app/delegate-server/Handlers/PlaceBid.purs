module DelegateServer.Handlers.PlaceBid
  ( PlaceBidError
      ( PlaceBidError_CouldNotDecodeBidTerms
      , PlaceBidError_ContractError
      )
  , PlaceBidResponse
  , PlaceBidSuccess
      ( PlaceBidSuccess_SubmittedTransaction
      )
  , placeBidHandler
  , placeBidHandlerImpl
  ) where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import DelegateServer.Contract.PlaceBid (PlaceBidL2ContractError, placeBidL2)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (class AppOpen)
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  , respCreatedOrBadRequest
  )
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Contract.Types (bidTermsCodec, contractErrorCodec, toContractError)
import HydraAuctionOffchain.Lib.Json (caDecodeString)

type PlaceBidResponse = ServerResponse PlaceBidSuccess PlaceBidError

data PlaceBidSuccess = PlaceBidSuccess_SubmittedTransaction

derive instance Generic PlaceBidSuccess _
derive instance Eq PlaceBidSuccess

instance Show PlaceBidSuccess where
  show = genericShow

instance EncodeJson PlaceBidSuccess where
  encodeJson = case _ of
    PlaceBidSuccess_SubmittedTransaction ->
      encodeJson
        { success: "Submitted PlaceBid L2 transaction."
        }

data PlaceBidError
  = PlaceBidError_CouldNotDecodeBidTerms String
  | PlaceBidError_ContractError PlaceBidL2ContractError

derive instance Generic PlaceBidError _
derive instance Eq PlaceBidError

instance Show PlaceBidError where
  show = genericShow

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
placeBidHandler ws = respCreatedOrBadRequest <=< placeBidHandlerImpl ws

placeBidHandlerImpl
  :: forall m
   . AppOpen m
  => HydraNodeApiWebSocket
  -> String
  -> m PlaceBidResponse
placeBidHandlerImpl ws bodyStr =
  case caDecodeString bidTermsCodec bodyStr of
    Left decodeErr ->
      pure $ ServerResponseError $
        PlaceBidError_CouldNotDecodeBidTerms decodeErr
    Right bidTerms -> do
      runExceptT (placeBidL2 ws bidTerms) <#> case _ of
        Left contractErr ->
          ServerResponseError $
            PlaceBidError_ContractError contractErr
        Right _ ->
          ServerResponseSuccess PlaceBidSuccess_SubmittedTransaction
