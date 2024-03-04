module DelegateServer.Lib.Json
  ( printJson
  , printJsonUsingCodec
  ) where

import Prelude

import Data.Argonaut (Json, stringifyWithIndent) as A
import Data.Codec.Argonaut (JsonCodec, encode) as CA

printJson :: A.Json -> String
printJson = A.stringifyWithIndent 2

printJsonUsingCodec :: forall (a :: Type). CA.JsonCodec a -> a -> String
printJsonUsingCodec codec = printJson <<< CA.encode codec
