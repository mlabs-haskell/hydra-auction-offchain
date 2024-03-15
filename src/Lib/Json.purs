module HydraAuctionOffchain.Lib.Json
  ( caDecodeString
  , caEncodeString
  , fromCaJsonDecodeError
  , writeJsonToFile
  ) where

import Prelude

import Data.Argonaut
  ( JsonDecodeError(TypeMismatch, UnexpectedValue, AtIndex, AtKey, Named, MissingValue)
  , parseJson
  , printJsonDecodeError
  , stringify
  ) as A
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut
  ( JsonCodec
  , JsonDecodeError(TypeMismatch, UnexpectedValue, AtIndex, AtKey, Named, MissingValue)
  , decode
  , encode
  , printJsonDecodeError
  ) as CA
import Data.Either (Either)
import Effect (Effect)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (writeTextFile) as FSSync
import Node.Path (FilePath)

caDecodeString :: forall a. CA.JsonCodec a -> String -> Either String a
caDecodeString codec jsonStr = do
  json <- lmap A.printJsonDecodeError $ A.parseJson jsonStr
  lmap CA.printJsonDecodeError $ CA.decode codec json

caEncodeString :: forall a. CA.JsonCodec a -> a -> String
caEncodeString codec = A.stringify <<< CA.encode codec

fromCaJsonDecodeError :: CA.JsonDecodeError -> A.JsonDecodeError
fromCaJsonDecodeError = case _ of
  CA.TypeMismatch type_ -> A.TypeMismatch type_
  CA.UnexpectedValue json -> A.UnexpectedValue json
  CA.AtIndex idx err -> A.AtIndex idx $ fromCaJsonDecodeError err
  CA.AtKey key err -> A.AtKey key $ fromCaJsonDecodeError err
  CA.Named name err -> A.Named name $ fromCaJsonDecodeError err
  CA.MissingValue -> A.MissingValue

writeJsonToFile :: forall a. EncodeJson a => FilePath -> a -> Effect Unit
writeJsonToFile fp =
  FSSync.writeTextFile Encoding.UTF8 fp
    <<< A.stringify
    <<< encodeJson
