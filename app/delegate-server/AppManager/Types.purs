module DelegateServer.AppManager.Types
  ( ActiveAuction
  , AppManager'(AppManager)
  , AuctionSlot
  ) where

import Cardano.Types (ScriptHash)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import DelegateServer.App (AppLogger, AppState)
import DelegateServer.Config (AppConfig)

type AuctionSlot = Int

type ActiveAuction (hydraNodeApiWs :: Type) =
  { appState :: AppState
  , appLogger :: AppLogger
  , hydraNodeApiWs :: hydraNodeApiWs
  , occupiedSlot :: AuctionSlot
  }

newtype AppManager' (hydraNodeApiWs :: Type) (wsServer :: Type) = AppManager
  { activeAuctions :: Map ScriptHash (ActiveAuction hydraNodeApiWs)
  , availableSlots :: Map AuctionSlot (AppConfig Maybe)
  , wsServer :: wsServer
  }

derive instance Newtype (AppManager' a b) _
