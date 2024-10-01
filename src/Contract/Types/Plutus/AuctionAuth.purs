module HydraAuctionOffchain.Contract.Types.Plutus.AuctionAuth
  ( AuctionAuth(AuctionAuth)
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Cardano.Types (PublicKey)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import HydraAuctionOffchain.Contract.Types.VerificationKey (VerificationKey)
import Type.Proxy (Proxy(Proxy))

newtype AuctionAuth = AuctionAuth
  { auctionCs :: CurrencySymbol
  , signatures :: Array (Tuple PublicKey ByteArray)
  }

derive instance Generic AuctionAuth _
derive instance Newtype AuctionAuth _
derive instance Eq AuctionAuth

instance Show AuctionAuth where
  show = genericShow

type AuctionAuthSchema =
  ("auctionCs" :~: CurrencySymbol)
    :$: ("signatures" :~: Array (Tuple PublicKey ByteArray))
    :$: Nil

auctionAuthSchema :: Proxy AuctionAuthSchema
auctionAuthSchema = Proxy

instance ToData AuctionAuth where
  toData (AuctionAuth rec) = Constr BigNum.zero $ toDataRec auctionAuthSchema rec

instance FromData AuctionAuth where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: Proxy AuctionAuth) == length pd =
        wrap <$> fromDataRec auctionAuthSchema pd
  fromData _ = Nothing
