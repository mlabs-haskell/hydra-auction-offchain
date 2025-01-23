module HydraAuctionOffchain.Service.DelegateServer
  ( getAvailableSlotsRequest
  , hostAuctionRequest
  , moveBidRequest
  , placeBidRequest
  , reserveSlotRequest
  ) where

import Prelude

import Cardano.Types (NetworkId, ScriptHash)
import Ctl.Internal.Helpers ((<</>>))
import Data.Array (singleton) as Array
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Set (Set)
import DelegateServer.Handlers.GetAvailableSlots (availableSlotsCodec)
import DelegateServer.Handlers.HostAuction
  ( HostAuctionRequest
  , HostAuctionResponse
  , hostAuctionRequestCodec
  , hostAuctionResponseCodec
  )
import DelegateServer.Handlers.MoveBid (MoveBidResponse, moveBidResponseCodec)
import DelegateServer.Handlers.PlaceBid (PlaceBidResponse, placeBidResponseCodec)
import DelegateServer.Handlers.ReserveSlot
  ( ReserveSlotRequest
  , ReserveSlotResponse
  , reserveSlotRequestCodec
  , reserveSlotResponseCodec
  )
import Effect.Aff (Aff)
import HydraAuctionOffchain.Contract.Types (BidTerms, bidTermsCodec)
import HydraAuctionOffchain.Service.Common (ServiceError)
import HydraAuctionOffchain.Service.DelegateServer.Http
  ( getRequest
  , handleResponse
  , mkAuctionCsHeader
  , postRequest
  )
import HydraSdk.Extra.AppManager (AppManagerSlot)

getAvailableSlotsRequest :: String -> Aff (Either ServiceError (Set AppManagerSlot))
getAvailableSlotsRequest httpServer =
  handleResponse availableSlotsCodec <$>
    getRequest (httpServer <</>> "availableSlots")

reserveSlotRequest
  :: String
  -> ReserveSlotRequest
  -> Aff (Either ServiceError ReserveSlotResponse)
reserveSlotRequest httpServer reqBody =
  handleResponse reserveSlotResponseCodec <$>
    postRequest
      { url: httpServer <</>> "reserveSlot"
      , content: Just $ CA.encode reserveSlotRequestCodec reqBody
      , headers: mempty
      }

hostAuctionRequest
  :: String
  -> HostAuctionRequest
  -> Aff (Either ServiceError HostAuctionResponse)
hostAuctionRequest httpServer reqBody =
  handleResponse hostAuctionResponseCodec <$>
    postRequest
      { url: httpServer <</>> "hostAuction"
      , content: Just $ CA.encode hostAuctionRequestCodec reqBody
      , headers: mempty
      }

moveBidRequest
  :: String
  -> NetworkId
  -> ScriptHash
  -> Aff (Either ServiceError MoveBidResponse)
moveBidRequest httpServer network auctionCs =
  handleResponse (moveBidResponseCodec network) <$>
    postRequest
      { url: httpServer <</>> "moveBid"
      , content: Nothing
      , headers: Array.singleton $ mkAuctionCsHeader auctionCs
      }

placeBidRequest
  :: String
  -> NetworkId
  -> BidTerms
  -> ScriptHash
  -> Aff (Either ServiceError PlaceBidResponse)
placeBidRequest httpServer network bidTerms auctionCs =
  handleResponse placeBidResponseCodec <$>
    postRequest
      { url: httpServer <</>> "placeBid"
      , content: Just $ CA.encode (bidTermsCodec network) bidTerms
      , headers: Array.singleton $ mkAuctionCsHeader auctionCs
      }
