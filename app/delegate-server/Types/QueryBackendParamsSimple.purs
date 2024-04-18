module DelegateServer.Types.QueryBackendParamsSimple
  ( BlockfrostBackendParamsSimple
  , QueryBackendParamsSimple(CtlBackendParams, BlockfrostBackendParams)
  , queryBackendParamsSimpleCodec
  , toQueryBackendParams
  ) where

import Prelude

import Contract.Config
  ( CtlBackendParams
  , QueryBackendParams
  , ServerConfig
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  )
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right))
import Data.Maybe (Maybe)
import Data.Profunctor (dimap)
import Data.Time.Duration (Seconds)
import Data.Variant (inj, match) as Variant
import DelegateServer.Lib.Codec (fixTaggedSumCodec)
import HydraAuctionOffchain.Codec (serverConfigCodec)
import Type.Proxy (Proxy(Proxy))

data QueryBackendParamsSimple
  = CtlBackendParams CtlBackendParams
  | BlockfrostBackendParams BlockfrostBackendParamsSimple

toQueryBackendParams :: QueryBackendParamsSimple -> Maybe Seconds -> QueryBackendParams
toQueryBackendParams backendParams confirmTxDelay =
  case backendParams of
    CtlBackendParams params -> mkCtlBackendParams params
    BlockfrostBackendParams params ->
      mkBlockfrostBackendParams
        { blockfrostConfig: params.config
        , blockfrostApiKey: params.apiKey
        , confirmTxDelay
        }

queryBackendParamsSimpleCodec :: CA.JsonCodec QueryBackendParamsSimple
queryBackendParamsSimpleCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "blockfrost": Right blockfrostBackendParamsSimpleCodec
          , "ctl": Right ctlBackendParamsCodec
          }
      )
  where
  toVariant = case _ of
    CtlBackendParams rec ->
      Variant.inj (Proxy :: _ "ctl") rec
    BlockfrostBackendParams rec ->
      Variant.inj (Proxy :: _ "blockfrost") rec

  fromVariant = Variant.match
    { "ctl": CtlBackendParams
    , "blockfrost": BlockfrostBackendParams
    }

type BlockfrostBackendParamsSimple =
  { config :: ServerConfig
  , apiKey :: Maybe String
  }

blockfrostBackendParamsSimpleCodec :: CA.JsonCodec BlockfrostBackendParamsSimple
blockfrostBackendParamsSimpleCodec =
  CA.object "BlockfrostBackendParamsSimple" $ CAR.record
    { config: serverConfigCodec
    , apiKey: CA.maybe CA.string
    }

ctlBackendParamsCodec :: CA.JsonCodec CtlBackendParams
ctlBackendParamsCodec =
  CA.object "CtlBackendParams" $ CAR.record
    { ogmiosConfig: serverConfigCodec
    , kupoConfig: serverConfigCodec
    }

