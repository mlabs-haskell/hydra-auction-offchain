module DelegateServer.AppManager.Types
  ( ActiveAuction
  , AppManager'(AppManager)
  , AuctionSlot
  , ReservedAuction
  , reserveSlot
  , withAppManager
  ) where

import Prelude

import Cardano.Types (ScriptHash)
import Data.Map (Map)
import Data.Map (insert, pop) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds, fromDuration)
import Data.Tuple (Tuple(Tuple), snd)
import Data.UUID (UUID, genUUID)
import DelegateServer.App (AppLogger, AppState)
import DelegateServer.Config (AppConfig)
import Effect.Aff (Aff, Fiber, delay, forkAff, generalBracket)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (put, take) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)

type AuctionSlot = Int

type ActiveAuction (hydraNodeApiWs :: Type) =
  { appState :: AppState
  , appLogger :: AppLogger
  , hydraNodeApiWs :: hydraNodeApiWs
  , occupiedSlot :: AuctionSlot
  }

type ReservedAuction =
  { appConfig :: AppConfig Maybe
  , reservationCode :: UUID
  }

newtype AppManager' (hydraNodeApiWs :: Type) (wsServer :: Type) = AppManager
  { activeAuctions :: Map ScriptHash (ActiveAuction hydraNodeApiWs)
  , reservedSlots :: Map AuctionSlot ReservedAuction
  , availableSlots :: Map AuctionSlot (AppConfig Maybe)
  , wsServer :: wsServer
  }

derive instance Newtype (AppManager' a b) _

withAppManager
  :: forall ws wsServer a
   . AVar (AppManager' ws wsServer)
  -> (AppManager' ws wsServer -> Aff (Tuple (AppManager' ws wsServer) a))
  -> Aff a
withAppManager appManagerAvar =
  map snd <<< generalBracket (AVar.take appManagerAvar)
    { killed: handleFailure
    , failed: handleFailure
    , completed:
        \(Tuple appManager' _) _ ->
          AVar.put appManager' appManagerAvar
    }
  where
  handleFailure :: Error -> AppManager' ws wsServer -> Aff Unit
  handleFailure = const (flip AVar.put appManagerAvar)

reserveSlot
  :: forall ws wsServer
   . AVar (AppManager' ws wsServer)
  -> Seconds
  -> AuctionSlot
  -> Aff (Maybe { reservationCode :: UUID })
reserveSlot appManagerAvar slotReservationPeriod slot =
  withAppManager appManagerAvar \(AppManager appManager) ->
    case Map.pop slot appManager.availableSlots of
      Nothing -> pure $ Tuple (AppManager appManager) Nothing
      Just (Tuple appConfig availableSlots) -> do
        void $ forkReservationMonitor appManagerAvar slotReservationPeriod slot
        reservationCode <- liftEffect genUUID
        pure $ Tuple
          ( AppManager $ appManager
              { availableSlots = availableSlots
              , reservedSlots =
                  Map.insert slot { appConfig, reservationCode }
                    appManager.reservedSlots
              }
          )
          (Just { reservationCode })

forkReservationMonitor
  :: forall ws wsServer
   . AVar (AppManager' ws wsServer)
  -> Seconds
  -> AuctionSlot
  -> Aff (Fiber Unit)
forkReservationMonitor appManagerAvar slotReservationPeriod slot =
  liftAff $ forkAff do
    delay $ fromDuration slotReservationPeriod
    withAppManager appManagerAvar \(AppManager appManager) ->
      case Map.pop slot appManager.reservedSlots of
        Nothing -> pure $ Tuple (AppManager appManager) unit
        Just (Tuple { appConfig } reservedSlots) -> do
          liftEffect $ log $ "Removed reservation for slot " <> show slot
          pure $ Tuple
            ( AppManager $ appManager
                { reservedSlots = reservedSlots
                , availableSlots = Map.insert slot appConfig appManager.availableSlots
                }
            )
            unit
