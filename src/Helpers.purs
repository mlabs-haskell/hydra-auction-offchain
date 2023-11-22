module HydraAuctionOffchain.Helpers
  ( tokenNameFromAsciiUnsafe
  ) where

import Prelude

import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.Value (TokenName, mkTokenName)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

tokenNameFromAsciiUnsafe :: String -> TokenName
tokenNameFromAsciiUnsafe tokenName =
  unsafePartial fromJust $ mkTokenName =<< byteArrayFromAscii tokenName
