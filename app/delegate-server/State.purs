module DelegateServer.State
  ( AppM
  , AppState
  , initApp
  , runApp
  , runAppEff
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Set (Set)
import Data.Set (empty) as Set
import DelegateServer.Config (AppConfig)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (new) as AVar

type AppM (a :: Type) = ReaderT AppState Aff a

type AppState =
  { config :: AppConfig
  , livePeersAVar :: AVar (Set String)
  }

initApp :: AppConfig -> Aff AppState
initApp config = do
  livePeersAVar <- AVar.new Set.empty
  pure
    { config
    , livePeersAVar
    }

runApp :: forall (a :: Type). AppState -> AppM a -> Aff a
runApp = flip runReaderT

runAppEff :: forall (a :: Type). AppState -> AppM a -> Effect Unit
runAppEff s = void <<< launchAff <<< flip runReaderT s
