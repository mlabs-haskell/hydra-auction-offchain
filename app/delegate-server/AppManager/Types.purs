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
import Data.Tuple (Tuple(Tuple))
import Data.UUID (UUID, genUUID)
import DelegateServer.App (AppLogger, AppState)
import DelegateServer.Config (AppConfig)
import Effect.Aff (Fiber, delay, forkAff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (put, take) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

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
  :: forall m ws wsServer a
   . MonadAff m
  => AVar (AppManager' ws wsServer)
  -> (AppManager' ws wsServer -> m (Tuple (AppManager' ws wsServer) a))
  -> m a
withAppManager appManagerAvar f = do
  appManager <- liftAff $ AVar.take appManagerAvar
  Tuple appManager' result <- f appManager
  liftAff $ AVar.put appManager' appManagerAvar
  pure result

reserveSlot
  :: forall m ws wsServer
   . MonadAff m
  => AVar (AppManager' ws wsServer)
  -> Seconds
  -> AuctionSlot
  -> m (Maybe { reservationCode :: UUID })
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
  :: forall m ws wsServer
   . MonadAff m
  => AVar (AppManager' ws wsServer)
  -> Seconds
  -> AuctionSlot
  -> m (Fiber Unit)
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
