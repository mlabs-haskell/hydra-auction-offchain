module DelegateServer.State
  ( becomeCommitLeader
  , initApp
  , readAppState
  , setAuctionInfo
  , setCollateralUtxo
  , setHeadStatus
  , setSnapshot
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
import Contract.Monad (mkContractEnv)
import Control.Monad.Reader (asks)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Set (empty) as Set
import DelegateServer.App (AppState, AppM)
import DelegateServer.Config (AppConfig)
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus(HeadStatus_Unknown))
import DelegateServer.Types.HydraSnapshot (HydraSnapshot, emptySnapshot)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (empty, new, read, tryPut) as AVar
import Effect.Aff.Class (liftAff)
import HydraAuctionOffchain.Contract.Types (AuctionInfoExtended, Utxo)

readAppState :: forall (a :: Type). (AppState -> AVar a) -> AppM a
readAppState f = (liftAff <<< AVar.read) =<< asks f

putAppState :: forall (a :: Type). (AppState -> AVar a) -> a -> AppM Unit
putAppState f val = asks f >>= (void <<< liftAff <<< AVar.tryPut val)

updAppState :: forall (a :: Type). (AppState -> AVar a) -> a -> AppM Unit
updAppState f val = asks f >>= (liftAff <<< flip modifyAVar_ (const (pure val)))

setAuctionInfo :: AuctionInfoExtended -> AppM Unit
setAuctionInfo = putAppState _.auctionInfo

setCollateralUtxo :: Utxo -> AppM Unit
setCollateralUtxo = putAppState _.collateralUtxo

setHeadStatus :: HydraHeadStatus -> AppM Unit
setHeadStatus = updAppState _.headStatus

setSnapshot :: HydraSnapshot -> AppM Unit
setSnapshot = updAppState _.snapshot

becomeCommitLeader :: AppM Unit
becomeCommitLeader = updAppState _.isCommitLeader true

whenCommitLeader :: AppM Unit -> AppM Unit
whenCommitLeader action = do
  avar <- asks _.isCommitLeader
  commitLeader <- liftAff $ AVar.read avar
  when commitLeader action

initApp :: AppConfig -> Aff AppState
initApp config = do
  contractEnv <- mkContractEnv $ mkContractParams config
  auctionInfo <- AVar.empty
  collateralUtxo <- AVar.empty
  headStatus <- AVar.new HeadStatus_Unknown
  snapshot <- AVar.new emptySnapshot
  livePeersAVar <- AVar.new Set.empty
  isCommitLeader <- AVar.new false
  pure
    { config
    , contractEnv
    , auctionInfo
    , collateralUtxo
    , headStatus
    , snapshot
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
  , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile config.walletSk) Nothing
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: disabledSynchronizationParams
  }
