module DelegateServer.HydraNodeApi.Types.DraftCommitTx
  ( DraftCommitTx
  , draftCommitTxCodec
  ) where

import Prelude

import Contract.Transaction (Transaction)
import Ctl.Internal.Deserialization.Transaction (deserializeTransaction)
import Ctl.Internal.Serialization (convertTransaction, toBytes)
import Data.Codec.Argonaut (JsonCodec, object, prismaticCodec, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (hush)
import Data.Newtype (unwrap, wrap)
import Effect.Unsafe (unsafePerformEffect)
import HydraAuctionOffchain.Codec (byteArrayCodec)

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

txCodec :: CA.JsonCodec Transaction
txCodec =
  CA.prismaticCodec
    "Transaction"
    (hush <<< deserializeTransaction <<< wrap)
    (unwrap <<< toBytes <<< unsafePerformEffect <<< convertTransaction)
    byteArrayCodec
