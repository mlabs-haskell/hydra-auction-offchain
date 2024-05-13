module DelegateServer.HydraNodeApi.Types.DraftCommitTx
  ( DraftCommitTx
  , draftCommitTxCodec
  ) where

import Prelude

import Contract.Transaction (Transaction)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import DelegateServer.Lib.Codec (txCodec)

type DraftCommitTx =
  { "type" :: String
  , description :: String
  , cborHex :: Transaction
  }

draftCommitTxCodec :: CA.JsonCodec DraftCommitTx
draftCommitTxCodec =
  CA.object "DraftCommitTx" $ CAR.record
    { "type": CA.string
    , description: CA.string
    , cborHex: txCodec
    }
