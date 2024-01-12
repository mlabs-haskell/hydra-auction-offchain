module HydraAuctionOffchain.Lib.Crypto
  ( hashVk
  ) where

import Prelude

import Contract.Address (PubKeyHash)
import Contract.Hashing (blake2b224Hash)
import Contract.Prim.ByteArray (ByteArray)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)

hashVk :: ByteArray -> Maybe PubKeyHash
hashVk = map wrap <<< ed25519KeyHashFromBytes <<< blake2b224Hash
