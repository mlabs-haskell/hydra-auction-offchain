module DelegateServer.HydraNodeApi.Types.Request where

import Prelude

import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left))
import Data.Profunctor (dimap)
import Data.Variant (inj, match) as Variant
import DelegateServer.Lib.Codec (fixTaggedSumCodec)
import Type.Proxy (Proxy(Proxy))

data HydraNodeApiRequest = HydraNodeApiRequest_Init

hydraNodeApiRequestCodec :: CA.JsonCodec HydraNodeApiRequest
hydraNodeApiRequestCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "Init": Left unit
          }
      )
  where
  toVariant = case _ of
    HydraNodeApiRequest_Init ->
      Variant.inj (Proxy :: Proxy "Init") unit

  fromVariant = Variant.match
    { "Init": const HydraNodeApiRequest_Init
    }
