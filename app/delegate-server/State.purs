module DelegateServer.State
  ( AppM
  , AppState
  , becomeCommitLeader
  , initApp
  , readAppState
  , runApp
  , runAppEff
  , runContract
  , runContractExitOnErr
  , runContractNullCosts
  , runContractNullCostsAff
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
import Contract.Monad (Contract, ContractEnv, mkContractEnv, runContractInEnv)
import Contract.Numeric.BigNum (one, zero) as BigNum
import Contract.ProtocolParameters (getProtocolParameters)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (modify)
import Data.Set (Set)
import Data.Set (empty) as Set
import Data.UInt (UInt)
import DelegateServer.Config (AppConfig)
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus(HeadStatus_Unknown))
import DelegateServer.Types.HydraSnapshot (HydraSnapshot)
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
  , Utxo
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

runContractNullCosts :: forall (a :: Type). Contract a -> AppM a
runContractNullCosts contract = do
  { contractEnv } <- ask
  liftAff $ runContractNullCostsAff contractEnv contract

runContractNullCostsAff :: forall (a :: Type). ContractEnv -> Contract a -> Aff a
runContractNullCostsAff contractEnv contract =
  runContractInEnv contractEnv do
    pparams <- getProtocolParameters <#> modify \rec ->
      rec
        { txFeeFixed = (zero :: UInt)
        , txFeePerByte = (zero :: UInt)
        , prices =
            { memPrice: { numerator: BigNum.zero, denominator: BigNum.one }
            , stepPrice: { numerator: BigNum.zero, denominator: BigNum.one }
            }
        }
    contract # local _
      { ledgerConstants =
          contractEnv.ledgerConstants
            { pparams = pparams
            }
      }

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
  , collateralUtxo :: AVar Utxo
  , headStatus :: AVar HydraHeadStatus
  , snapshot :: AVar HydraSnapshot
  , livePeersAVar :: AVar (Set String)
  , isCommitLeader :: AVar Boolean
  }

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
  snapshot <- AVar.new mempty
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
