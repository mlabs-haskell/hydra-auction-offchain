module HydraAuctionOffchain.Lib.ToData
  ( serializeData
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.ToData (class ToData, toData)
import Cardano.Types (CborBytes)

serializeData :: forall a. ToData a => a -> CborBytes
serializeData = encodeCbor <<< toData
