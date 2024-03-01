module DelegateServer.Lib.Json
  ( printJson
  ) where

import Prelude

import Data.Argonaut (stringifyWithIndent) as A
import Data.Codec.Argonaut (JsonCodec, encode) as CA

printJson :: forall (a :: Type). CA.JsonCodec a -> a -> String
printJson codec =
  A.stringifyWithIndent 2 <<< CA.encode codec
