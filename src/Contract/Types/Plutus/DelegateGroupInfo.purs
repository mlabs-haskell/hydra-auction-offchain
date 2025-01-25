module HydraAuctionOffchain.Contract.Types.Plutus.DelegateGroupInfo
  ( DelegateGroupInfo(DelegateGroupInfo)
  , delegateGroupInfoCodec
  ) where

import Prelude

import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData)
import Cardano.Types (Ed25519KeyHash, PlutusData(Constr), ScriptHash)
import Cardano.Types.BigNum (zero) as BigNum
import Data.Codec.Argonaut (JsonCodec, array, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (ed25519KeyHashCodec, scriptHashCodec)
import HydraAuctionOffchain.Contract.Types.Plutus.DelegateInfo
  ( DelegateInfo
  , delegateInfoCodec
  )
import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
  ( type (:$:)
  , type (:~:)
  , Nil
  , fromDataRec
  , recLength
  , toDataRec
  )
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import Type.Proxy (Proxy(Proxy))

newtype DelegateGroupInfo = DelegateGroupInfo
  { delegateGroupId :: ScriptHash
  , delegateGroupMasterKeys :: Array Ed25519KeyHash
  , delegateGroupServers :: DelegateInfo
  , delegateGroupMetadata :: String
  }

derive instance Generic DelegateGroupInfo _
derive instance Newtype DelegateGroupInfo _
derive instance Eq DelegateGroupInfo

instance Show DelegateGroupInfo where
  show = genericShow

type DelegateGroupInfoSchema =
  ("delegateGroupId" :~: ScriptHash)
    :$: ("delegateGroupMasterKeys" :~: Array Ed25519KeyHash)
    :$: ("delegateGroupServers" :~: DelegateInfo)
    :$: ("delegateGroupMetadata" :~: String)
    :$: Nil

delegateInfoSchema :: Proxy DelegateGroupInfoSchema
delegateInfoSchema = Proxy

instance ToData DelegateGroupInfo where
  toData (DelegateGroupInfo rec) =
    Constr BigNum.zero $ toDataRec delegateInfoSchema rec

instance FromData DelegateGroupInfo where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: _ DelegateGroupInfo) == length pd =
        wrap <$> fromDataRec delegateInfoSchema pd
  fromData _ = Nothing

instance HasJson DelegateGroupInfo anyParams where
  jsonCodec _ = const delegateGroupInfoCodec

delegateGroupInfoCodec :: CA.JsonCodec DelegateGroupInfo
delegateGroupInfoCodec =
  wrapIso DelegateGroupInfo $ CA.object "DelegateGroupInfo" $ CAR.record
    { delegateGroupId: scriptHashCodec
    , delegateGroupMasterKeys: CA.array ed25519KeyHashCodec
    , delegateGroupServers: delegateInfoCodec
    , delegateGroupMetadata: CA.string
    }
