module HydraAuctionOffchain.Types.HostPort
  ( HostPort
  , hostPortCodec
  , parseHostPort
  , printHostPort
  , readHostPort
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, prismaticCodec, string) as CA
import Data.Either (note)
import Data.Maybe (Maybe(Nothing))
import Data.String (Pattern(Pattern))
import Data.String (split) as String
import Data.UInt (UInt)
import Data.UInt (fromString, toString) as UInt
import Options.Applicative (ReadM, eitherReader) as Optparse

type HostPort = { host :: String, port :: UInt }

hostPortCodec :: CA.JsonCodec HostPort
hostPortCodec = CA.prismaticCodec "HostPort" readHostPort printHostPort CA.string

printHostPort :: HostPort -> String
printHostPort { host, port } = host <> ":" <> UInt.toString port

readHostPort :: String -> Maybe HostPort
readHostPort str =
  case String.split (Pattern ":") str of
    [ host, port ] -> { host, port: _ } <$> UInt.fromString port
    _ -> Nothing

parseHostPort :: Optparse.ReadM HostPort
parseHostPort =
  Optparse.eitherReader $ \str ->
    note ("Can't parse as HostPort: `" <> str <> "`") $ readHostPort str

