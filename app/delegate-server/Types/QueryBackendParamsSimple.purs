module DelegateServer.Types.QueryBackendParamsSimple
  ( BlockfrostBackendParamsSimple
  , QueryBackendParamsSimple(CtlBackendParams, BlockfrostBackendParams)
  , queryBackendParamsSimpleCodec
  , toQueryBackendParams
  ) where

import Contract.Config
  ( CtlBackendParams
  , QueryBackendParams
  , ServerConfig
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  )
import Data.Codec.Argonaut (JsonCodec, JPropCodec, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds)
import HydraAuctionOffchain.Codec (serverConfigCodec)

data QueryBackendParamsSimple
  = CtlBackendParams CtlBackendParams
  | BlockfrostBackendParams BlockfrostBackendParamsSimple

derive instance Generic QueryBackendParamsSimple _

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
  CAS.sumFlat "QueryBackendParamsSimple"
    { "BlockfrostBackendParams": blockfrostBackendParamsSimpleCodec
    , "CtlBackendParams": ctlBackendParamsCodec
    }

type BlockfrostBackendParamsSimple =
  { config :: ServerConfig
  , apiKey :: Maybe String
  }

blockfrostBackendParamsSimpleCodec :: CA.JPropCodec BlockfrostBackendParamsSimple
blockfrostBackendParamsSimpleCodec =
  CAR.record
    { config: serverConfigCodec
    , apiKey: CA.maybe CA.string
    }

ctlBackendParamsCodec :: CA.JPropCodec CtlBackendParams
ctlBackendParamsCodec =
  CAR.record
    { ogmiosConfig: serverConfigCodec
    , kupoConfig: serverConfigCodec
    }
