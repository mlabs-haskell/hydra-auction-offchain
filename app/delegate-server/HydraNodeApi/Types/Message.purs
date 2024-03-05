module DelegateServer.HydraNodeApi.Types.Message
  ( GreetingsMessage
  , HydraNodeApi_InMessage
      ( In_Greetings
      , In_PeerConnected
      , In_PeerDisconnected
      , In_HeadIsInitializing
      , In_Committed
      )
  , HydraNodeApi_OutMessage
      ( Out_Init
      )
  , PeerConnMessage
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Profunctor (dimap)
import Data.Variant (inj, match) as Variant
import DelegateServer.Lib.Codec (fixTaggedSumCodec)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus, headStatusCodec)
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- Incoming messages

data HydraNodeApi_InMessage
  = In_Greetings GreetingsMessage
  | In_PeerConnected PeerConnMessage
  | In_PeerDisconnected PeerConnMessage
  | In_HeadIsInitializing
  | In_Committed

hydraNodeApiInMessageCodec :: CA.JsonCodec HydraNodeApi_InMessage
hydraNodeApiInMessageCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Greetings": Right greetingsMessageCodec
          , "PeerConnected": Right peerConnMessageCodec
          , "PeerDisconnected": Right peerConnMessageCodec
          , "HeadIsInitializing": Left unit
          , "Committed": Left unit
          }
      )
  where
  toVariant = case _ of
    In_Greetings rec ->
      Variant.inj (Proxy :: Proxy "Greetings") rec
    In_PeerConnected rec ->
      Variant.inj (Proxy :: Proxy "PeerConnected") rec
    In_PeerDisconnected rec ->
      Variant.inj (Proxy :: Proxy "PeerDisconnected") rec
    In_HeadIsInitializing ->
      Variant.inj (Proxy :: Proxy "HeadIsInitializing") unit
    In_Committed ->
      Variant.inj (Proxy :: Proxy "Committed") unit

  fromVariant = Variant.match
    { "Greetings": In_Greetings
    , "PeerConnected": In_PeerConnected
    , "PeerDisconnected": In_PeerDisconnected
    , "HeadIsInitializing": const In_HeadIsInitializing
    , "Committed": const In_Committed
    }

type PeerConnMessage =
  { peer :: String
  }

peerConnMessageCodec :: CA.JsonCodec PeerConnMessage
peerConnMessageCodec =
  CA.object "PeerConnMessage" $ CAR.record
    { peer: CA.string
    }

type GreetingsMessage =
  { headStatus :: HydraHeadStatus
  }

greetingsMessageCodec :: CA.JsonCodec GreetingsMessage
greetingsMessageCodec =
  CA.object "GreetingsMessage" $ CAR.record
    { headStatus: headStatusCodec
    }

----------------------------------------------------------------------
-- Outcoming messages

data HydraNodeApi_OutMessage = Out_Init

hydraNodeApiOutMessageCodec :: CA.JsonCodec HydraNodeApi_OutMessage
hydraNodeApiOutMessageCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Init": Left unit
          }
      )
  where
  toVariant = case _ of
    Out_Init ->
      Variant.inj (Proxy :: Proxy "Init") unit

  fromVariant = Variant.match
    { "Init": const Out_Init
    }
