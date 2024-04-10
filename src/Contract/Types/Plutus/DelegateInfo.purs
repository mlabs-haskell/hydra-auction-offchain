module HydraAuctionOffchain.Contract.Types.Plutus.DelegateInfo
  ( DelegateInfo(DelegateInfo)
  , delegateInfoCodec
  , randomHttpServer
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Data.Codec.Argonaut (JsonCodec, array, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect)
import HydraAuctionOffchain.Helpers (randomElem)
import Type.Proxy (Proxy(Proxy))

newtype DelegateInfo = DelegateInfo
  { httpServers :: Array String
  , wsServers :: Array String
  }

derive instance Generic DelegateInfo _
derive instance Newtype DelegateInfo _
derive instance Eq DelegateInfo

instance Show DelegateInfo where
  show = genericShow

type DelegateInfoSchema =
  ("httpServers" :~: Array String)
    :$: ("wsServers" :~: Array String)
    :$: Nil

delegateInfoSchema :: Proxy DelegateInfoSchema
delegateInfoSchema = Proxy

instance ToData DelegateInfo where
  toData (DelegateInfo rec) = Constr BigNum.zero $ toDataRec delegateInfoSchema rec

instance FromData DelegateInfo where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: Proxy DelegateInfo) == length pd =
        wrap <$> fromDataRec delegateInfoSchema pd
  fromData _ = Nothing

delegateInfoCodec :: CA.JsonCodec DelegateInfo
delegateInfoCodec =
  wrapIso DelegateInfo $ CA.object "DelegateInfo" $ CAR.record
    { httpServers: CA.array CA.string
    , wsServers: CA.array CA.string
    }

randomHttpServer :: forall m. MonadEffect m => DelegateInfo -> m String
randomHttpServer = randomElem <<< _.httpServers <<< unwrap
