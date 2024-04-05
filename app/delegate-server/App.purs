module DelegateServer.App
  ( AppM(AppM)
  , AppState
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
  , LogLevel(Trace)
  , NetworkId(TestnetId, MainnetId)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
  , defaultTimeParams
  , disabledSynchronizationParams
  , emptyHooks
  )
import Contract.Monad (Contract, mkContractEnv, runContractInEnv)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set (empty) as Set
import DelegateServer.Config (AppConfig, Network(Testnet, Mainnet))
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

newtype AppM (a :: Type) = AppM (ReaderT AppState Aff a)

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

runApp :: forall a. AppState -> AppM a -> Aff a
runApp s = flip runReaderT s <<< unwrap

runAppEff :: forall a. AppState -> AppM a -> Effect Unit
runAppEff s = void <<< launchAff <<< flip runReaderT s <<< unwrap

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
  , exitSem :: AVar AppExitReason
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

instance MonadAccess AppM "exitSem" (AVar AppExitReason) where
  access _ = asks _.exitSem

instance MonadAccess AppM "collateralUtxo" (AVar Utxo) where
  access _ = asks _.collateralUtxo

instance MonadAccess AppM "commitStatus" (AVar CommitStatus) where
  access _ = asks _.commitStatus

instance MonadAccess AppM "snapshot" (AVar HydraSnapshot) where
  access _ = asks _.snapshot

initApp :: AppConfig -> Aff AppState
initApp config = do
  contractEnv <- wrap <$> mkContractEnv (mkContractParams config)
  auctionInfo <- AVar.empty
  headStatus <- AVar.new HeadStatus_Unknown
  livePeers <- AVar.new Set.empty
  exitSem <- AVar.empty
  collateralUtxo <- AVar.empty
  commitStatus <- AVar.new ShouldCommitCollateral
  snapshot <- AVar.new emptySnapshot
  pure
    { config
    , contractEnv
    , auctionInfo
    , headStatus
    , livePeers
    , exitSem
    , collateralUtxo
    , commitStatus
    , snapshot
    }

mkContractParams :: AppConfig -> ContractParams
mkContractParams config =
  { backendParams: (unwrap config).queryBackend
  , networkId:
      case (unwrap config).network of
        Testnet _ -> TestnetId
        Mainnet -> MainnetId
  , logLevel: Trace
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile (unwrap config).walletSk) Nothing
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: disabledSynchronizationParams
  }
