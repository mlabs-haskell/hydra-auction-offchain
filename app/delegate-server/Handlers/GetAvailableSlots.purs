module DelegateServer.Handlers.GetAvailableSlots
  ( availableSlotsCodec
  , getAvailableSlotsHandler
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, int) as CA
import Data.Set (Set)
import DelegateServer.AppManager.Types (AppManager', AuctionSlot)
import DelegateServer.AppManager.Types (getAvailableSlots) as AppManager
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import HTTPure (Response, ok) as HTTPure
import HydraAuctionOffchain.Codec (setCodec)
import HydraAuctionOffchain.Lib.Json (caEncodeString)

getAvailableSlotsHandler
  :: forall ws wsServer
   . AVar (AppManager' ws wsServer)
  -> Aff HTTPure.Response
getAvailableSlotsHandler =
  (HTTPure.ok <<< caEncodeString availableSlotsCodec)
    <=< AppManager.getAvailableSlots

availableSlotsCodec :: CA.JsonCodec (Set AuctionSlot)
availableSlotsCodec = setCodec CA.int
