module HydraAuctionOffchain.Lib.Json
  ( caDecodeFile
  , caEncodeString
  , printJson
  , printJsonUsingCodec
  ) where

import Prelude

import Aeson (Aeson, stringifyAeson)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, encode) as CA
import Data.Either (Either)
import Effect (Effect)
import HydraSdk.Lib (caDecodeString)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (readTextFile) as FSSync
import Node.Path (FilePath)

foreign import stringifyAesonWithIndent :: Int -> Aeson -> String

caDecodeFile :: forall a. CA.JsonCodec a -> FilePath -> Effect (Either CA.JsonDecodeError a)
caDecodeFile codec =
  map (caDecodeString codec)
    <<< FSSync.readTextFile Encoding.UTF8

caEncodeString :: forall a. CA.JsonCodec a -> a -> String
caEncodeString codec = stringifyAeson <<< CA.encode codec

printJson :: Aeson -> String
printJson = stringifyAesonWithIndent 2

printJsonUsingCodec :: forall (a :: Type). CA.JsonCodec a -> a -> String
printJsonUsingCodec codec = printJson <<< CA.encode codec
