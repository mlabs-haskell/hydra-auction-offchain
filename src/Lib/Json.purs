module HydraAuctionOffchain.Lib.Json
  ( caDecodeString
  , caEncodeString
  ) where

import Prelude

import Data.Argonaut (parseJson, printJsonDecodeError, stringify) as A
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, decode, encode, printJsonDecodeError) as CA
import Data.Either (Either)

caDecodeString :: forall a. CA.JsonCodec a -> String -> Either String a
caDecodeString codec jsonStr = do
  json <- lmap A.printJsonDecodeError $ A.parseJson jsonStr
  lmap CA.printJsonDecodeError $ CA.decode codec json

caEncodeString :: forall a. CA.JsonCodec a -> a -> String
caEncodeString codec = A.stringify <<< CA.encode codec
