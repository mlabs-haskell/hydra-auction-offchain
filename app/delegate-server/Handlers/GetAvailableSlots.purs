module DelegateServer.Handlers.GetAvailableSlots
  ( availableSlotsCodec
  , getAvailableSlotsHandler
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, int) as CA
import Data.Set (Set)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import HTTPure (Response, ok) as HTTPure
import HydraAuctionOffchain.Codec (setCodec)
import HydraAuctionOffchain.Lib.Json (caEncodeString)
import HydraSdk.Extra.AppManager (AppManager, AppManagerSlot)
import HydraSdk.Extra.AppManager (getAvailableSlots) as AppManager

getAvailableSlotsHandler
  :: forall appId appState appConfigAvailable appConfigActive
   . AVar (AppManager appId appState appConfigAvailable appConfigActive)
  -> Aff HTTPure.Response
getAvailableSlotsHandler =
  (HTTPure.ok <<< caEncodeString availableSlotsCodec)
    <=< AppManager.getAvailableSlots

availableSlotsCodec :: CA.JsonCodec (Set AppManagerSlot)
availableSlotsCodec = setCodec CA.int
