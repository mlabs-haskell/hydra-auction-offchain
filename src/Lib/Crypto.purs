module HydraAuctionOffchain.Lib.Crypto
  ( hashVk
  , verifySignature
  ) where

import Prelude

import Cardano.Types (Ed25519KeyHash)
import Cardano.Types.PublicKey (fromRawBytes, hash) as PublicKey
import Contract.Prim.ByteArray (ByteArray)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Effect (Effect)

foreign import verifySignature :: ByteArray -> ByteArray -> ByteArray -> Effect Boolean

hashVk :: ByteArray -> Maybe Ed25519KeyHash
hashVk = map PublicKey.hash <<< PublicKey.fromRawBytes <<< wrap
