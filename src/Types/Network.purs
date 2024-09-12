module HydraAuctionOffchain.Types.Network
  ( Network(Preview, Preprod, Mainnet)
  , blockfrostPublicServerConfig
  , networkCodec
  , toCtlNetworkId
  ) where

import Prelude

import Cardano.Types (NetworkId(TestnetId, MainnetId))
import Contract.Config
  ( ServerConfig
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
  )
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Network = Preview | Preprod | Mainnet

derive instance Generic Network _

instance Show Network where
  show = genericShow

networkCodec :: CA.JsonCodec Network
networkCodec = CAG.nullarySum "Network"

toCtlNetworkId :: Network -> NetworkId
toCtlNetworkId = case _ of
  Preview -> TestnetId
  Preprod -> TestnetId
  Mainnet -> MainnetId

blockfrostPublicServerConfig :: Network -> ServerConfig
blockfrostPublicServerConfig = case _ of
  Preview -> blockfrostPublicPreviewServerConfig
  Preprod -> blockfrostPublicPreprodServerConfig
  Mainnet -> blockfrostPublicMainnetServerConfig
