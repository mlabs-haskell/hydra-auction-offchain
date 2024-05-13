module DelegateServer.Lib.Timer
  ( scheduleAt
  , setTimeout
  ) where

import Prelude

import Contract.Time (POSIXTime(POSIXTime))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (empty, tryPut, tryTake) as AVar
import Effect.Timer (TimeoutId)
import Effect.Timer (setTimeout) as Timer
import HydraAuctionOffchain.Helpers (fromJustWithErr, nowPosix)
import JS.BigInt (fromInt, toInt) as BigInt

scheduleAt :: POSIXTime -> Effect Unit -> Effect (AVar (Maybe TimeoutId))
scheduleAt at action = do
  nowTime <- nowPosix
  timerIdAvar <- AVar.empty
  setTimeout (at - nowTime) action timerIdAvar
  pure timerIdAvar

-- `timerIdAvar` is expected to be empty on function invocation
setTimeout :: POSIXTime -> Effect Unit -> AVar (Maybe TimeoutId) -> Effect Unit
setTimeout (POSIXTime timeout) action timerIdAvar
  | timeout <= zero =
      action *> void (AVar.tryPut Nothing timerIdAvar)
  | timeout > BigInt.fromInt top = do
      timerId <- Timer.setTimeout top do
        void $ AVar.tryTake timerIdAvar
        setTimeout (wrap $ timeout - BigInt.fromInt top) action timerIdAvar
      void $ AVar.tryPut (Just timerId) timerIdAvar
  | otherwise = do
      timerId <- Timer.setTimeout (fromJustWithErr "setTimeout" $ BigInt.toInt timeout)
        action
      void $ AVar.tryPut (Just timerId) timerIdAvar
