module DelegateServer.Helpers
  ( modifyF
  , printOref
  , readOref
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Contract.CborBytes (cborBytesToHex, hexToCborBytes)
import Contract.Prim.ByteArray (byteLength)
import Contract.Transaction (TransactionInput(TransactionInput))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(Pattern))
import Data.String (split) as String
import Data.UInt (fromString, toString) as UInt

modifyF :: forall t a f. Newtype t a => Functor f => (a -> f a) -> t -> f t
modifyF f t = wrap <$> f (unwrap t)

printOref :: TransactionInput -> String
printOref (TransactionInput rec) =
  cborBytesToHex (encodeCbor rec.transactionId) <> "#" <> UInt.toString rec.index

readOref :: String -> Maybe TransactionInput
readOref str =
  case String.split (Pattern "#") str of
    [ txHashStr, idx ]
      | Just transactionId <- decodeCbor =<< hexToCborBytes txHashStr
      , Just index <- UInt.fromString idx ->
          Just $ wrap { transactionId, index }
    _ ->
      Nothing
