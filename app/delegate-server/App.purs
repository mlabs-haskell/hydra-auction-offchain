module DelegateServer.App
  ( AppLogger
  , AppM(AppM)
  , AppState
  , appLoggerDefault
  , getAppEffRunner
  , getAppRunner
  , initApp
  , runApp
  , runAppEff
  , runContract
  , runContractExitOnErr
  , runContractLift
  , runContractNullCosts
  ) where

import Prelude

import Contract.Config
  ( ContractParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , QueryBackendParams
  , WalletSpec(UseKeys)
  , defaultTimeParams
  , disabledSynchronizationParams
  , emptyHooks
  )
import Contract.Monad (Contract, mkContractEnv, runContractInEnv)
import Contract.Value (CurrencySymbol)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Logger.Trans (class MonadLogger, LoggerT(LoggerT), runLoggerT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set (empty) as Set
import DelegateServer.Config
  ( AppConfig
  , AppConfig'(AppConfig)
  , AuctionConfig
  , networkToNetworkId
  )
import DelegateServer.Lib.Contract (runContractNullCostsAff)
import DelegateServer.State
  ( class App
  , class AppBase
  , class AppInit
  , class AppOpen
  , class MonadAccess
  , ContractEnvWrapper
  , access
  )
import DelegateServer.Types.AppExitReason (AppExitReason)
import DelegateServer.Types.CommitStatus (CommitStatus(ShouldCommitCollateral))
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus(HeadStatus_Unknown))
import DelegateServer.Types.HydraSnapshot (HydraSnapshot, emptySnapshot)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (empty, new) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended
  , ContractOutput(ContractOutputError, ContractOutputResult)
  , Utxo
  , contractErrorCodec
  )
import HydraAuctionOffchain.Lib.Json (caEncodeString)
import Node.Process (exit)
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- AppM

newtype AppM (a :: Type) = AppM (LoggerT (ReaderT AppState Aff) a)

instance AppBase AppM
instance AppInit AppM
instance AppOpen AppM
instance App AppM

derive instance Newtype (AppM a) _
derive newtype instance Functor AppM
derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadAsk AppState AppM
derive newtype instance MonadReader AppState AppM
derive newtype instance MonadThrow Error AppM
derive newtype instance MonadError Error AppM
derive newtype instance MonadLogger AppM
derive newtype instance MonadRec AppM

type AppLogger = Message -> ReaderT AppState Aff Unit

appLoggerDefault :: AppLogger
appLoggerDefault message = do
  appLogLevel <- ask <#> _.logLevel <<< unwrap <<< _.config
  when (message.level >= appLogLevel) do
    messageFormatted <- prettyFormatter message
    liftEffect $ log messageFormatted

runApp :: forall a. AppState -> AppLogger -> AppM a -> Aff a
runApp appState logger =
  flip runReaderT appState
    <<< flip runLoggerT logger
    <<< unwrap

runAppEff :: forall a. AppState -> AppLogger -> AppM a -> Effect Unit
runAppEff appState logger =
  void
    <<< launchAff
    <<< runApp appState logger

getAppRunner :: AppM (forall a. AppM a -> Aff a)
getAppRunner =
  wrap $
    LoggerT \logger ->
      ask <#> \appState ->
        runApp appState logger

getAppEffRunner :: AppM (forall a. AppM a -> Effect Unit)
getAppEffRunner =
  wrap $
    LoggerT \logger ->
      ask <#> \appState ->
        runAppEff appState logger

runContract :: forall m a. AppBase m => Contract a -> m a
runContract contract = do
  contractEnv <- access (Proxy :: _ "contractEnv")
  liftAff $ runContractInEnv (unwrap contractEnv) contract

runContractLift :: forall t m a. MonadTrans t => AppBase m => Contract a -> t m a
runContractLift = lift <<< runContract

runContractExitOnErr :: forall m a. AppBase m => Contract (ContractOutput a) -> m a
runContractExitOnErr =
  runContract >=> case _ of
    ContractOutputResult res -> pure res
    ContractOutputError err ->
      liftEffect do
        log $ caEncodeString contractErrorCodec err
        exit one

runContractNullCosts :: forall m a. AppBase m => Contract a -> m a
runContractNullCosts contract = do
  contractEnv <- access (Proxy :: _ "contractEnv")
  liftAff $ runContractNullCostsAff (unwrap contractEnv) contract

----------------------------------------------------------------------
-- AppState

type AppState =
  { config :: AppConfig
  , contractEnv :: ContractEnvWrapper
  , auctionInfo :: AVar AuctionInfoExtended
  , headStatus :: AVar HydraHeadStatus
  , livePeers :: AVar (Set String)
  , exit :: AVar (AppExitReason -> Effect Unit)
  , headCs :: AVar CurrencySymbol
  , collateralUtxo :: AVar Utxo
  , commitStatus :: AVar CommitStatus
  , snapshot :: AVar HydraSnapshot
  }

instance MonadAccess AppM "config" AppConfig where
  access _ = asks _.config

instance MonadAccess AppM "contractEnv" ContractEnvWrapper where
  access _ = asks _.contractEnv

instance MonadAccess AppM "auctionInfo" (AVar AuctionInfoExtended) where
  access _ = asks _.auctionInfo

instance MonadAccess AppM "headStatus" (AVar HydraHeadStatus) where
  access _ = asks _.headStatus

instance MonadAccess AppM "livePeers" (AVar (Set String)) where
  access _ = asks _.livePeers

instance MonadAccess AppM "exit" (AVar (AppExitReason -> Effect Unit)) where
  access _ = asks _.exit

instance MonadAccess AppM "headCs" (AVar CurrencySymbol) where
  access _ = asks _.headCs

instance MonadAccess AppM "collateralUtxo" (AVar Utxo) where
  access _ = asks _.collateralUtxo

instance MonadAccess AppM "commitStatus" (AVar CommitStatus) where
  access _ = asks _.commitStatus

instance MonadAccess AppM "snapshot" (AVar HydraSnapshot) where
  access _ = asks _.snapshot

initApp :: forall ac. AppConfig' ac QueryBackendParams -> AuctionConfig -> Aff AppState
initApp (AppConfig appConfig) auctionConfig = do
  contractEnv <- wrap <$> mkContractEnv contractParams
  auctionInfo <- AVar.empty
  headStatus <- AVar.new HeadStatus_Unknown
  livePeers <- AVar.new Set.empty
  exit <- AVar.empty
  headCs <- AVar.empty
  collateralUtxo <- AVar.empty
  commitStatus <- AVar.new ShouldCommitCollateral
  snapshot <- AVar.new emptySnapshot
  pure
    { config: wrap $ appConfig { auctionConfig = auctionConfig }
    , contractEnv
    , auctionInfo
    , headStatus
    , livePeers
    , exit
    , headCs
    , collateralUtxo
    , commitStatus
    , snapshot
    }
  where
  contractParams :: ContractParams
  contractParams =
    { backendParams: appConfig.queryBackend
    , networkId: networkToNetworkId appConfig.network
    , logLevel: appConfig.ctlLogLevel
    , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile auctionConfig.cardanoSk) Nothing
    , customLogger: Nothing
    , suppressLogs: true
    , hooks: emptyHooks
    , timeParams: defaultTimeParams
    , synchronizationParams: disabledSynchronizationParams
    }
