module DelegateServer.State
  ( class AccessRec
  , class App
  , class AppBase
  , class AppInit
  , class AppOpen
  , class MonadAccess
  , ContractEnvWrapper(ContractEnvWrapper)
  , access
  , accessRec
  , readAppState
  , setAuctionInfo
  , setCollateralUtxo
  , setCommitStatus
  , setHeadStatus
  , setSnapshot
  ) where

import Prelude

import Contract.Monad (ContractEnv)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Symbol (class IsSymbol)
import DelegateServer.Config (AppConfig)
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.Types.CommitStatus (CommitStatus)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus)
import DelegateServer.Types.HydraSnapshot (HydraSnapshot)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (read, tryPut) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error)
import HydraAuctionOffchain.Contract.Types (AuctionInfoExtended, Utxo)
import Prim.Row (class Cons, class Lacks)
import Record (insert) as Record
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(Proxy))

newtype ContractEnvWrapper = ContractEnvWrapper ContractEnv

derive instance Newtype ContractEnvWrapper _

class MonadAccess :: (Type -> Type) -> Symbol -> Type -> Constraint
class (Monad m) <= MonadAccess m l a | m l -> a where
  access :: Proxy l -> m a

instance (MonadTrans t, Monad (t m), MonadAccess m l a) => MonadAccess (t m) l a where
  access = lift <<< access

class AccessRec :: (Type -> Type) -> List' Symbol -> Row Type -> Constraint
class AccessRec m ls r | m ls -> r where
  accessRec :: Proxy ls -> m (Record r)

instance Applicative m => AccessRec m Nil' () where
  accessRec _ = pure {}

instance
  ( MonadAccess m l a
  , AccessRec m ls r'
  , Cons l a r' r
  , Lacks l r'
  , IsSymbol l
  ) =>
  AccessRec m (l :> ls) r where
  accessRec _ = do
    let label = (Proxy :: Proxy l)
    Record.insert label <$> access label <*> accessRec (Proxy :: Proxy ls)

-- FIXME: This instance breaks types for some reason.
-- With this instance enabled, AppBase m will require MonadAsk r m.
--
-- instance (MonadAsk (Record r) m, IsSymbol l, Cons l a r' r) => MonadAccess m l a where
--   access l = Record.get l <$> ask

class
  ( MonadAff m
  , MonadThrow Error m
  , MonadAccess m "config" AppConfig
  , MonadAccess m "contractEnv" ContractEnvWrapper
  , MonadAccess m "auctionInfo" (AVar AuctionInfoExtended)
  , MonadAccess m "headStatus" (AVar HydraHeadStatus)
  , MonadAccess m "livePeers" (AVar (Set String))
  ) <=
  AppBase m

-- We don't need access to the hydra snapshot until the head is open.
class
  ( AppBase m
  , MonadAccess m "collateralUtxo" (AVar Utxo)
  , MonadAccess m "commitStatus" (AVar CommitStatus)
  ) <=
  AppInit m

-- We don't need access to commit-related info once the head is open.
class
  ( AppBase m
  , MonadAccess m "snapshot" (AVar HydraSnapshot)
  ) <=
  AppOpen m

class
  ( AppBase m
  , AppInit m
  , AppOpen m
  ) <=
  App m

readAppState
  :: forall m l a
   . MonadAccess m l (AVar a)
  => MonadAff m
  => Proxy l
  -> m a
readAppState l =
  (liftAff <<< AVar.read)
    =<< access l

putAppState
  :: forall m l a
   . MonadAccess m l (AVar a)
  => MonadAff m
  => Proxy l
  -> a
  -> m Unit
putAppState l val =
  (void <<< liftAff <<< AVar.tryPut val)
    =<< access l

updAppState
  :: forall m l a
   . MonadAccess m l (AVar a)
  => MonadAff m
  => Proxy l
  -> a
  -> m Unit
updAppState l val =
  (liftAff <<< flip modifyAVar_ (const (pure val)))
    =<< access l

setAuctionInfo
  :: forall m
   . MonadAccess m "auctionInfo" (AVar AuctionInfoExtended)
  => MonadAff m
  => AuctionInfoExtended
  -> m Unit
setAuctionInfo = putAppState (Proxy :: _ "auctionInfo")

setCollateralUtxo
  :: forall m
   . MonadAccess m "collateralUtxo" (AVar Utxo)
  => MonadAff m
  => Utxo
  -> m Unit
setCollateralUtxo = putAppState (Proxy :: _ "collateralUtxo")

setHeadStatus
  :: forall m
   . MonadAccess m "headStatus" (AVar HydraHeadStatus)
  => MonadAff m
  => HydraHeadStatus
  -> m Unit
setHeadStatus = updAppState (Proxy :: _ "headStatus")

setSnapshot
  :: forall m
   . MonadAccess m "snapshot" (AVar HydraSnapshot)
  => MonadAff m
  => HydraSnapshot
  -> m Unit
setSnapshot = updAppState (Proxy :: _ "snapshot")

setCommitStatus
  :: forall m
   . MonadAccess m "commitStatus" (AVar CommitStatus)
  => MonadAff m
  => CommitStatus
  -> m Unit
setCommitStatus = updAppState (Proxy :: _ "commitStatus")
