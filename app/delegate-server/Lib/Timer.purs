module DelegateServer.Lib.Timer
  ( scheduleAt
  , setTimeout
  ) where

import Prelude

import Contract.Time (POSIXTime(POSIXTime))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Timer (setTimeout) as Timer
import HydraAuctionOffchain.Helpers (fromJustWithErr, nowPosix)
import JS.BigInt (fromInt, toInt) as BigInt

scheduleAt :: POSIXTime -> Effect Unit -> Effect Unit
scheduleAt at action = do
  nowTime <- nowPosix
  setTimeout (at - nowTime) action

setTimeout :: POSIXTime -> Effect Unit -> Effect Unit
setTimeout (POSIXTime timeout) action
  | timeout <= zero =
      action
  | timeout > BigInt.fromInt top =
      void $ Timer.setTimeout top $ void $
        setTimeout (wrap $ timeout - BigInt.fromInt top)
          action
  | otherwise =
      void $ Timer.setTimeout (fromJustWithErr "setTimeout" $ BigInt.toInt timeout)
        action
