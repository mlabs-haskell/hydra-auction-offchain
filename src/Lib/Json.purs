module HydraAuctionOffchain.Lib.Json
  ( caDecodeFile
  , caDecodeString
  , caEncodeString
  , fromCaJsonDecodeError
  , readJsonFromFile
  , writeJsonToFile
  ) where

import Prelude

import Data.Argonaut
  ( JsonDecodeError(TypeMismatch, UnexpectedValue, AtIndex, AtKey, Named, MissingValue)
  , parseJson
  , printJsonDecodeError
  , stringify
  ) as A
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
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
import Node.FS.Sync (readTextFile, writeTextFile) as FSSync
import Node.Path (FilePath)

caDecodeFile :: forall a. CA.JsonCodec a -> FilePath -> Effect (Either String a)
caDecodeFile codec =
  map (caDecodeString codec)
    <<< FSSync.readTextFile Encoding.UTF8

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

readJsonFromFile :: forall a. DecodeJson a => FilePath -> Effect (Either String a)
readJsonFromFile =
  map (lmap A.printJsonDecodeError <<< (decodeJson <=< A.parseJson))
    <<< FSSync.readTextFile Encoding.UTF8

writeJsonToFile :: forall a. EncodeJson a => FilePath -> a -> Effect Unit
writeJsonToFile fp =
  FSSync.writeTextFile Encoding.UTF8 fp
    <<< A.stringify
    <<< encodeJson
