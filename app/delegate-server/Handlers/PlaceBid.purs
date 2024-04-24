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
  , placeBidResponseCodec
  ) where

import Prelude

import Contract.Address (getNetworkId)
import Control.Monad.Except (runExceptT)
import Data.Codec.Argonaut (JsonCodec, string) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Variant (inj, match) as Variant
import DelegateServer.App (runContract)
import DelegateServer.Contract.PlaceBid
  ( PlaceBidL2ContractError
  , placeBidL2
  , placeBidL2ContractErrorCodec
  )
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (class AppOpen)
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Contract.Types (bidTermsCodec)
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import Type.Proxy (Proxy(Proxy))

type PlaceBidResponse = ServerResponse PlaceBidSuccess PlaceBidError

placeBidResponseCodec :: CA.JsonCodec PlaceBidResponse
placeBidResponseCodec = serverResponseCodec placeBidSuccessCodec placeBidErrorCodec

placeBidHandler :: forall m. AppOpen m => HydraNodeApiWebSocket -> String -> m HTTPure.Response
placeBidHandler ws bidTerms = do
  resp <- placeBidHandlerImpl ws bidTerms
  respCreatedOrBadRequest placeBidResponseCodec resp

placeBidHandlerImpl
  :: forall m
   . AppOpen m
  => HydraNodeApiWebSocket
  -> String
  -> m PlaceBidResponse
placeBidHandlerImpl ws bodyStr = do
  network <- runContract getNetworkId
  case caDecodeString (bidTermsCodec network) bodyStr of
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

-- PlaceBidSuccess ---------------------------------------------------

data PlaceBidSuccess = PlaceBidSuccess_SubmittedTransaction

derive instance Generic PlaceBidSuccess _
derive instance Eq PlaceBidSuccess

instance Show PlaceBidSuccess where
  show = genericShow

instance HasJson PlaceBidSuccess anyParams where
  jsonCodec _ = const placeBidSuccessCodec

placeBidSuccessCodec :: CA.JsonCodec PlaceBidSuccess
placeBidSuccessCodec =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "SubmittedTransaction": Left unit
        }
    )
  where
  toVariant = case _ of
    PlaceBidSuccess_SubmittedTransaction ->
      Variant.inj (Proxy :: _ "SubmittedTransaction") unit

  fromVariant = Variant.match
    { "SubmittedTransaction": const PlaceBidSuccess_SubmittedTransaction
    }

-- PlaceBidError -----------------------------------------------------

data PlaceBidError
  = PlaceBidError_CouldNotDecodeBidTerms String
  | PlaceBidError_ContractError PlaceBidL2ContractError

derive instance Generic PlaceBidError _
derive instance Eq PlaceBidError

instance Show PlaceBidError where
  show = genericShow

instance HasJson PlaceBidError anyParams where
  jsonCodec _ = const placeBidErrorCodec

placeBidErrorCodec :: CA.JsonCodec PlaceBidError
placeBidErrorCodec =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "CouldNotDecodeBidTerms": Right CA.string
        , "ContractError": Right placeBidL2ContractErrorCodec
        }
    )
  where
  toVariant = case _ of
    PlaceBidError_CouldNotDecodeBidTerms x ->
      Variant.inj (Proxy :: _ "CouldNotDecodeBidTerms") x
    PlaceBidError_ContractError x ->
      Variant.inj (Proxy :: _ "ContractError") x

  fromVariant = Variant.match
    { "CouldNotDecodeBidTerms": PlaceBidError_CouldNotDecodeBidTerms
    , "ContractError": PlaceBidError_ContractError
    }
