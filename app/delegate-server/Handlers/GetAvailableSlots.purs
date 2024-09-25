module DelegateServer.Handler.GetAvailableSlots
  ( getAvailableSlotsHandler
  ) where

import Prelude

import Data.Codec.Argonaut (array, int) as CA
import DelegateServer.AppManager.Types (AppManager')
import DelegateServer.AppManager.Types (getAvailableSlots) as AppManager
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import HTTPure (Response, ok) as HTTPure
import HydraAuctionOffchain.Lib.Json (caEncodeString)

getAvailableSlotsHandler
  :: forall ws wsServer
   . AVar (AppManager' ws wsServer)
  -> Aff HTTPure.Response
getAvailableSlotsHandler =
  (HTTPure.ok <<< caEncodeString (CA.array CA.int))
    <=< AppManager.getAvailableSlots
