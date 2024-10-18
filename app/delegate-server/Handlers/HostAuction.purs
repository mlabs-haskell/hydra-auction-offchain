module DelegateServer.Handlers.HostAuction
  ( HostAuctionError
      ( CouldNotDecodeHostAuctionReqBody
      , AuctionSlotNotAvailable
      , IncorrectReservationCode
      , MissingOrInvalidAuctionInfo
      )
  , HostAuctionRequest
  , HostAuctionResponse
  , hostAuctionHandler
  , hostAuctionHandlerImpl
  , hostAuctionRequestCodec
  , hostAuctionResponseCodec
  ) where

import Prelude

import Cardano.Types (TransactionInput)
import Control.Monad.Except (ExceptT(ExceptT), except, runExceptT)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, int, null, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(Tuple))
import DelegateServer.AppManager (AppManager')
import DelegateServer.AppManager
  ( HostAuctionError
      ( AuctionSlotNotAvailable
      , IncorrectReservationCode
      , MissingOrInvalidAuctionInfo
      )
  , hostAuction
  ) as AppManager
import DelegateServer.Helpers (printOref, readOref)
import DelegateServer.Types.ServerResponse
  ( ServerResponse
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import DelegateServer.Types.ServerResponse (fromEither) as ServerResponse
import DelegateServer.WsServer (DelegateWebSocketServer)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Codec (uuidCodec)
import HydraAuctionOffchain.Contract.Types (ContractError, contractErrorCodec, toContractError)
import HydraAuctionOffchain.Lib.Codec (sumGenericCodec)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import HydraSdk.Extra.AppManager (ReservationCode, AppManagerSlot, withAppManager)

hostAuctionHandler
  :: AVar AppManager'
  -> DelegateWebSocketServer
  -> String
  -> Aff HTTPure.Response
hostAuctionHandler appManagerAvar wsServer bodyStr =
  hostAuctionHandlerImpl appManagerAvar wsServer bodyStr >>=
    respCreatedOrBadRequest hostAuctionResponseCodec
      <<< ServerResponse.fromEither

hostAuctionHandlerImpl
  :: AVar AppManager'
  -> DelegateWebSocketServer
  -> String
  -> Aff (Either HostAuctionError Unit)
hostAuctionHandlerImpl appManagerAvar wsServer bodyStr =
  runExceptT do
    reqBody <- except $ lmap CouldNotDecodeHostAuctionReqBody $
      caDecodeString hostAuctionRequestCodec bodyStr
    ExceptT $ lmap convertHostAuctionError <$>
      withAppManager appManagerAvar \appManager ->
        either (Tuple appManager <<< Left) (flip Tuple (Right unit)) <$>
          AppManager.hostAuction
            { slot: reqBody.slot
            , reservationCode: reqBody.reservationCode
            , appManager
            , appManagerAvar
            , auctionMetadataOref: reqBody.auctionMetadataOref
            , wsServer
            }

-- HostAuctionRequest ------------------------------------------------

type HostAuctionRequest =
  { auctionMetadataOref :: TransactionInput
  , slot :: AppManagerSlot
  , reservationCode :: Maybe ReservationCode
  }

hostAuctionRequestCodec :: CA.JsonCodec HostAuctionRequest
hostAuctionRequestCodec =
  CA.object "HostAuctionRequest" $ CAR.record
    { auctionMetadataOref:
        CA.prismaticCodec "TransactionInput" readOref printOref
          CA.string
    , slot: CA.int
    , reservationCode: CA.maybe uuidCodec
    }

-- HostAuctionResponse -----------------------------------------------

type HostAuctionResponse = ServerResponse Unit HostAuctionError

hostAuctionResponseCodec :: CA.JsonCodec HostAuctionResponse
hostAuctionResponseCodec = serverResponseCodec CA.null hostAuctionErrorCodec

-- HostAuctionError --------------------------------------------------

data HostAuctionError
  = CouldNotDecodeHostAuctionReqBody String
  | AuctionSlotNotAvailable
  | IncorrectReservationCode
  | MissingOrInvalidAuctionInfo ContractError

derive instance Generic HostAuctionError _
derive instance Eq HostAuctionError

instance Show HostAuctionError where
  show = genericShow

hostAuctionErrorCodec :: CA.JsonCodec HostAuctionError
hostAuctionErrorCodec =
  sumGenericCodec "ReserveSlotError"
    ( CAV.variantMatch
        { "CouldNotDecodeHostAuctionReqBody": Right CA.string
        , "AuctionSlotNotAvailable": Left unit
        , "IncorrectReservationCode": Left unit
        , "MissingOrInvalidAuctionInfo": Right contractErrorCodec
        }
    )

convertHostAuctionError :: AppManager.HostAuctionError -> HostAuctionError
convertHostAuctionError = case _ of
  AppManager.AuctionSlotNotAvailable ->
    AuctionSlotNotAvailable
  AppManager.IncorrectReservationCode ->
    IncorrectReservationCode
  AppManager.MissingOrInvalidAuctionInfo queryAuctionErr ->
    MissingOrInvalidAuctionInfo $
      toContractError queryAuctionErr
