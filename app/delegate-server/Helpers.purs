module DelegateServer.Helpers
  ( printOref
  , readOref
  ) where

import Prelude

import Contract.Prim.ByteArray (byteArrayToHex, byteLength, hexToByteArray)
import Contract.Transaction (TransactionInput(TransactionInput))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(Pattern))
import Data.String (split) as String
import Data.UInt (fromString, toString) as UInt

printOref :: TransactionInput -> String
printOref (TransactionInput rec) =
  byteArrayToHex (unwrap rec.transactionId) <> "#" <> UInt.toString rec.index

readOref :: String -> Maybe TransactionInput
readOref str =
  case String.split (Pattern "#") str of
    [ txHashStr, idx ]
      | Just txHash <- hexToByteArray txHashStr
      , byteLength txHash == 32
      , Just index <- UInt.fromString idx ->
          Just $ wrap { transactionId: wrap txHash, index }
    _ ->
      Nothing
