module HydraAuctionOffchain.Contract.Types.ContractError
  ( class ToContractError
  , ContractError(ContractError)
  , contractErrorCodec
  , toContractError
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)

class ToContractError a where
  toContractError :: a -> ContractError

newtype ContractError = ContractError
  { errorCode :: String
  , message :: String
  }

derive instance Generic ContractError _
derive instance Newtype ContractError _
derive instance Eq ContractError

instance Show ContractError where
  show = genericShow

contractErrorCodec :: CA.JsonCodec ContractError
contractErrorCodec =
  wrapIso ContractError $ CA.object "ContractError" $ CAR.record
    { errorCode: CA.string
    , message: CA.string
    }
