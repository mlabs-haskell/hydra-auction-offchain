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
  , reserveSlotRequestCodec
  , reserveSlotResponseCodec
  ) where

import Prelude

import Cardano.Types (Ed25519KeyHash)
import Control.Error.Util ((!?))
import Control.Monad.Except (except, runExceptT)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, int, object, printJsonDecodeError, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sum) as CAS
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber) as Int
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds(Seconds))
import DelegateServer.Config (AppConfig)
import DelegateServer.Types.ServerResponse
  ( ServerResponse
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import DelegateServer.Types.ServerResponse (fromEither) as ServerResponse
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Codec (ed25519KeyHashCodec, uuidCodec)
import HydraSdk.Extra.AppManager (AppManager, AppManagerSlot, ReservationCode)
import HydraSdk.Extra.AppManager (reserveSlot) as AppManager
import HydraSdk.Lib (caDecodeString)

reserveSlotHandler
  :: forall f appId appState appConfigActive
   . AVar (AppManager appId appState (AppConfig f) appConfigActive)
  -> Int
  -> String
  -> Aff HTTPure.Response
reserveSlotHandler appManagerAvar slotReservationPeriod bodyStr =
  reserveSlotHandlerImpl appManagerAvar slotReservationPeriod bodyStr >>=
    respCreatedOrBadRequest reserveSlotResponseCodec
      <<< ServerResponse.fromEither

reserveSlotHandlerImpl
  :: forall f appId appState appConfigActive
   . AVar (AppManager appId appState (AppConfig f) appConfigActive)
  -> Int
  -> String
  -> Aff (Either ReserveSlotError ReserveSlotSuccess)
reserveSlotHandlerImpl appManagerAvar slotReservationPeriod bodyStr =
  runExceptT do
    reqBody <- except $ lmap (CouldNotDecodeReserveSlotReqBody <<< CA.printJsonDecodeError) $
      caDecodeString reserveSlotRequestCodec bodyStr
    let
      logger = liftEffect <<< log
      slotReservationPeriodSec = Seconds $ Int.toNumber slotReservationPeriod
    res <- AppManager.reserveSlot appManagerAvar slotReservationPeriodSec reqBody.slot logger
      !? RequestedSlotNotAvailable
    pure
      { reservationCode: res.reservationCode
      , delegatePkh: (unwrap res.config).auctionConfig.delegatePkh
      }

-- ReserveSlotRequest ------------------------------------------------

type ReserveSlotRequest =
  { slot :: AppManagerSlot
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
  { reservationCode :: ReservationCode
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
  CAS.sum "ReserveSlotError"
    { "CouldNotDecodeReserveSlotReqBody": CA.string
    , "RequestedSlotNotAvailable": unit
    }
