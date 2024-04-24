module HydraAuctionOffchain.Lib.Optparse
  ( jsonReader
  , parserReader
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec) as CA
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import Options.Applicative (ReadM, eitherReader) as Optparse
import Parsing (Parser, runParser)

jsonReader :: forall (a :: Type). String -> CA.JsonCodec a -> Optparse.ReadM a
jsonReader typeName codec =
  Optparse.eitherReader $ \str ->
    caDecodeString codec str # lmap \err ->
      "Can't parse as " <> typeName <> ": `" <> str <> "` ~ " <> err

parserReader :: forall a. String -> Parser String a -> Optparse.ReadM a
parserReader typeName parser =
  Optparse.eitherReader $ \str ->
    runParser str parser # lmap \err ->
      "Can't parse as " <> typeName <> ": `" <> str <> "` ~ " <> show err
