module DelegateServer.App
  ( AppM
  , AppState
  , runApp
  , runAppEff
  , runContract
  , runContractExitOnErr
  , runContractNullCosts
  ) where

import Prelude

import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Set (Set)
import DelegateServer.Config (AppConfig)
import DelegateServer.Lib.Contract (runContractNullCostsAff)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus)
import DelegateServer.Types.HydraSnapshot (HydraSnapshot)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended
  , ContractOutput(ContractOutputError, ContractOutputResult)
  , Utxo
  , contractErrorCodec
  )
import HydraAuctionOffchain.Lib.Json (caEncodeString)
import Node.Process (exit)

type AppM (a :: Type) = ReaderT AppState Aff a

type AppState =
  { config :: AppConfig
  , contractEnv :: ContractEnv
  , auctionInfo :: AVar AuctionInfoExtended
  , collateralUtxo :: AVar Utxo
  , headStatus :: AVar HydraHeadStatus
  , snapshot :: AVar HydraSnapshot
  , livePeersAVar :: AVar (Set String)
  , isCommitLeader :: AVar Boolean
  }

runApp :: forall (a :: Type). AppState -> AppM a -> Aff a
runApp = flip runReaderT

runAppEff :: forall (a :: Type). AppState -> AppM a -> Effect Unit
runAppEff s = void <<< launchAff <<< flip runReaderT s

runContract :: forall (a :: Type). Contract a -> AppM a
runContract contract = do
  { contractEnv } <- ask
  liftAff $ runContractInEnv contractEnv contract

runContractExitOnErr :: forall (a :: Type). Contract (ContractOutput a) -> AppM a
runContractExitOnErr =
  runContract >=> case _ of
    ContractOutputResult res -> pure res
    ContractOutputError err ->
      liftEffect do
        log $ caEncodeString contractErrorCodec err
        exit one

runContractNullCosts :: forall (a :: Type). Contract a -> AppM a
runContractNullCosts contract = do
  { contractEnv } <- ask
  liftAff $ runContractNullCostsAff contractEnv contract
