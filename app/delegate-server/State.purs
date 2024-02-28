module DelegateServer.State
  ( AppM
  , AppState
  , askHeadStatus
  , becomeCommitLeader
  , initApp
  , runApp
  , runAppEff
  , runContract
  , setAuctionInfo
  , setHeadStatus
  ) where

import Prelude

import Contract.Config
  ( ContractParams
  , LogLevel(Trace)
  , NetworkId(TestnetId)
  , blockfrostPublicPreprodServerConfig
  , defaultConfirmTxDelay
  , defaultTimeParams
  , disabledSynchronizationParams
  , emptyHooks
  , mkBlockfrostBackendParams
  )
import Contract.Monad (Contract, ContractEnv, mkContractEnv, runContractInEnv)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Set (Set)
import Data.Set (empty) as Set
import DelegateServer.Config (AppConfig)
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus(HeadStatus_Unknown))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (empty, new, read, tryPut) as AVar
import Effect.Aff.Class (liftAff)
import HydraAuctionOffchain.Contract.Types (AuctionInfoExtended)

type AppM (a :: Type) = ReaderT AppState Aff a

runApp :: forall (a :: Type). AppState -> AppM a -> Aff a
runApp = flip runReaderT

runAppEff :: forall (a :: Type). AppState -> AppM a -> Effect Unit
runAppEff s = void <<< launchAff <<< flip runReaderT s

runContract :: forall (a :: Type). Contract a -> AppM a
runContract contract = do
  { contractEnv } <- ask
  liftAff $ runContractInEnv contractEnv contract

type AppState =
  { config :: AppConfig
  , contractEnv :: ContractEnv
  , auctionInfo :: AVar AuctionInfoExtended
  , headStatus :: AVar HydraHeadStatus
  , livePeersAVar :: AVar (Set String)
  , isCommitLeader :: AVar Boolean
  }

setAuctionInfo :: AuctionInfoExtended -> AppM Unit
setAuctionInfo auctionInfo =
  asks _.auctionInfo >>=
    (void <<< liftAff <<< AVar.tryPut auctionInfo)

askHeadStatus :: AppM HydraHeadStatus
askHeadStatus = (liftAff <<< AVar.read) =<< asks _.headStatus

setHeadStatus :: HydraHeadStatus -> AppM Unit
setHeadStatus headStatus =
  asks _.headStatus >>=
    (liftAff <<< flip modifyAVar_ (const (pure headStatus)))

becomeCommitLeader :: AppM Unit
becomeCommitLeader =
  asks _.isCommitLeader >>=
    (liftAff <<< flip modifyAVar_ (const (pure true)))

initApp :: AppConfig -> Aff AppState
initApp config = do
  contractEnv <- mkContractEnv $ mkContractParams config.blockfrostApiKey
  auctionInfo <- AVar.empty
  headStatus <- AVar.new HeadStatus_Unknown
  livePeersAVar <- AVar.new Set.empty
  isCommitLeader <- AVar.new false
  pure
    { config
    , contractEnv
    , auctionInfo
    , headStatus
    , livePeersAVar
    , isCommitLeader
    }

mkContractParams :: String -> ContractParams
mkContractParams blockfrostApiKey =
  { backendParams:
      mkBlockfrostBackendParams
        { blockfrostConfig: blockfrostPublicPreprodServerConfig
        , blockfrostApiKey: Just blockfrostApiKey
        , confirmTxDelay: defaultConfirmTxDelay
        }
  , networkId: TestnetId
  , logLevel: Trace
  , walletSpec: Nothing
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: disabledSynchronizationParams
  }
