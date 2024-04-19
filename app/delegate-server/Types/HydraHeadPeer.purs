module DelegateServer.Types.HydraHeadPeer
  ( HydraHeadPeer
  , hydraHeadPeerCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import HydraAuctionOffchain.Types.HostPort (HostPort, hostPortCodec)
import Node.Path (FilePath)

type HydraHeadPeer =
  { hydraNode :: HostPort
  , hydraVk :: FilePath
  , cardanoVk :: FilePath
  }

hydraHeadPeerCodec :: CA.JsonCodec HydraHeadPeer
hydraHeadPeerCodec =
  CA.object "HydraHeadPeer" $ CAR.record
    { hydraNode: hostPortCodec
    , hydraVk: CA.string
    , cardanoVk: CA.string
    }
