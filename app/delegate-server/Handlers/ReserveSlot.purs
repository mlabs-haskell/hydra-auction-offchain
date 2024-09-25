module DelegateServer.Handlers.ReserveSlot
  ( ReserveSlotError
      ( CouldNotDecodeReserveSlotReqBody
      , RequestedSlotNotAvailable
      )
  , ReserveSlotRequest
  , ReserveSlotResponse
  , ReserveSlotSuccess
  , reserveSlotHandler
  , reserveSlotHandlerImpl
  ) where

import Prelude

import Cardano.Types (Ed25519KeyHash)
import Control.Error.Util ((!?))
import Control.Monad.Except (except, runExceptT)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, int, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds)
import Data.UUID (UUID)
import DelegateServer.AppManager.Types (AppManager', AuctionSlot)
import DelegateServer.AppManager.Types (reserveSlot) as AppManager
import DelegateServer.Types.ServerResponse
  ( ServerResponse
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import DelegateServer.Types.ServerResponse (fromEither) as ServerResponse
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Codec (ed25519KeyHashCodec, uuidCodec)
import HydraAuctionOffchain.Lib.Codec (sumGenericCodec)
import HydraAuctionOffchain.Lib.Json (caDecodeString)

reserveSlotHandler
  :: forall ws wsServer
   . AVar (AppManager' ws wsServer)
  -> Seconds
  -> String
  -> Aff HTTPure.Response
reserveSlotHandler appManagerAvar slotReservationPeriod bodyStr =
  reserveSlotHandlerImpl appManagerAvar slotReservationPeriod bodyStr >>=
    respCreatedOrBadRequest reserveSlotResponseCodec
      <<< ServerResponse.fromEither

reserveSlotHandlerImpl
  :: forall ws wsServer
   . AVar (AppManager' ws wsServer)
  -> Seconds
  -> String
  -> Aff (Either ReserveSlotError ReserveSlotSuccess)
reserveSlotHandlerImpl appManagerAvar slotReservationPeriod bodyStr =
  runExceptT do
    reqBody <- except $ lmap CouldNotDecodeReserveSlotReqBody $
      caDecodeString reserveSlotRequestCodec bodyStr
    AppManager.reserveSlot appManagerAvar slotReservationPeriod reqBody.slot !?
      RequestedSlotNotAvailable

-- ReserveSlotRequest ------------------------------------------------

type ReserveSlotRequest =
  { slot :: AuctionSlot
  }

reserveSlotRequestCodec :: CA.JsonCodec ReserveSlotRequest
reserveSlotRequestCodec =
  CA.object "ReserveSlotRequest" $ CAR.record
    { slot: CA.int
    }

-- ReserveSlotResponse -----------------------------------------------

type ReserveSlotResponse = ServerResponse ReserveSlotSuccess ReserveSlotError

reserveSlotResponseCodec :: CA.JsonCodec ReserveSlotResponse
reserveSlotResponseCodec = serverResponseCodec reserveSlotSuccessCodec reserveSlotErrorCodec

-- ReserveSlotSuccess ------------------------------------------------

type ReserveSlotSuccess =
  { reservationCode :: UUID
  , delegatePkh :: Ed25519KeyHash
  }

reserveSlotSuccessCodec :: CA.JsonCodec ReserveSlotSuccess
reserveSlotSuccessCodec =
  CA.object "ReserveSlotSuccess" $ CAR.record
    { reservationCode: uuidCodec
    , delegatePkh: ed25519KeyHashCodec
    }

-- ReserveSlotError --------------------------------------------------

data ReserveSlotError
  = CouldNotDecodeReserveSlotReqBody String
  | RequestedSlotNotAvailable

derive instance Generic ReserveSlotError _
derive instance Eq ReserveSlotError

instance Show ReserveSlotError where
  show = genericShow

reserveSlotErrorCodec :: CA.JsonCodec ReserveSlotError
reserveSlotErrorCodec =
  sumGenericCodec "ReserveSlotError"
    ( CAV.variantMatch
        { "CouldNotDecodeReserveSlotReqBody": Right CA.string
        , "RequestedSlotNotAvailable": Left unit
        }
    )
