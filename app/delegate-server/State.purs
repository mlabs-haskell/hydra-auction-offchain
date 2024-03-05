module DelegateServer.State
  ( AppM
  , AppState
  , askAuctionInfo
  , askHeadStatus
  , becomeCommitLeader
  , initApp
  , runApp
  , runAppEff
  , runContract
  , runContractExitOnErr
  , setAuctionInfo
  , setHeadStatus
  , whenCommitLeader
  ) where

import Prelude

import Contract.Config
  ( ContractParams
  , LogLevel(Trace)
  , NetworkId(TestnetId)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
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
import Effect.Class (liftEffect)
import Effect.Console (log)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended
  , ContractOutput(ContractOutputError, ContractOutputResult)
  , contractErrorCodec
  )
import HydraAuctionOffchain.Lib.Json (caEncodeString)
import Node.Process (exit)

type AppM (a :: Type) = ReaderT AppState Aff a

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

askAuctionInfo :: AppM AuctionInfoExtended
askAuctionInfo = (liftAff <<< AVar.read) =<< asks _.auctionInfo

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

whenCommitLeader :: AppM Unit -> AppM Unit
whenCommitLeader action = do
  avar <- asks _.isCommitLeader
  commitLeader <- liftAff $ AVar.read avar
  when commitLeader action

initApp :: AppConfig -> Aff AppState
initApp config = do
  contractEnv <- mkContractEnv $ mkContractParams config
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

mkContractParams :: AppConfig -> ContractParams
mkContractParams config =
  { backendParams:
      mkBlockfrostBackendParams
        { blockfrostConfig: blockfrostPublicPreprodServerConfig
        , blockfrostApiKey: Just config.blockfrostApiKey
        , confirmTxDelay: defaultConfirmTxDelay
        }
  , networkId: TestnetId
  , logLevel: Trace
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile config.cardanoSk) Nothing
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: disabledSynchronizationParams
  }
