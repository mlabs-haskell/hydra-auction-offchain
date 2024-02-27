module DelegateServer.HydraNodeApi.Types.Message
  ( HydraNodeApi_InMsg
      ( HydraNodeApi_InMsg_PeerConnected
      , HydraNodeApi_InMsg_PeerDisconnected
      )
  , HydraNodeApi_OutMsg(HydraNodeApi_OutMsg_Init)
  , hydraNodeApiInMsgCodec
  , hydraNodeApiOutMsgCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Profunctor (dimap)
import Data.Variant (inj, match) as Variant
import DelegateServer.Lib.Codec (fixTaggedSumCodec)
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- Incoming messages

data HydraNodeApi_InMsg
  = HydraNodeApi_InMsg_PeerConnected { peer :: String }
  | HydraNodeApi_InMsg_PeerDisconnected { peer :: String }

hydraNodeApiInMsgCodec :: CA.JsonCodec HydraNodeApi_InMsg
hydraNodeApiInMsgCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "PeerConnected":
              Right $ CA.object "PeerConnected" $ CAR.record
                { peer: CA.string
                }
          , "PeerDisconnected":
              Right $ CA.object "PeerDisconnected" $ CAR.record
                { peer: CA.string
                }
          }
      )
  where
  toVariant = case _ of
    HydraNodeApi_InMsg_PeerConnected rec ->
      Variant.inj (Proxy :: Proxy "PeerConnected") rec
    HydraNodeApi_InMsg_PeerDisconnected rec ->
      Variant.inj (Proxy :: Proxy "PeerDisconnected") rec

  fromVariant = Variant.match
    { "PeerConnected": HydraNodeApi_InMsg_PeerConnected
    , "PeerDisconnected": HydraNodeApi_InMsg_PeerDisconnected
    }

----------------------------------------------------------------------
-- Outcoming messages

data HydraNodeApi_OutMsg = HydraNodeApi_OutMsg_Init

hydraNodeApiOutMsgCodec :: CA.JsonCodec HydraNodeApi_OutMsg
hydraNodeApiOutMsgCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Init": Left unit
          }
      )
  where
  toVariant = case _ of
    HydraNodeApi_OutMsg_Init ->
      Variant.inj (Proxy :: Proxy "Init") unit

  fromVariant = Variant.match
    { "Init": const HydraNodeApi_OutMsg_Init
    }
