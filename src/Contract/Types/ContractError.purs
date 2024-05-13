module HydraAuctionOffchain.Contract.Types.ContractError
  ( class GenericConstrIndex
  , class ToContractError
  , ContractError(ContractError)
  , contractErrorCodec
  , errorCodePrefix
  , errorMessage
  , genericConstrIndex
  , toContractError
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (Constructor, Sum(Inl, Inr), from) as Generic
import Data.Generic.Rep (class Generic)
import Data.Int (decimal, toStringAs) as Int
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Type.Proxy (Proxy(Proxy))

constrIndex :: forall a rep. Generic a rep => GenericConstrIndex rep => a -> Int
constrIndex = genericConstrIndex <<< Generic.from

class GenericConstrIndex rep where
  genericConstrIndex :: rep -> Int

instance GenericConstrIndex (Generic.Constructor name a) where
  genericConstrIndex = const zero

instance GenericConstrIndex b => GenericConstrIndex (Generic.Sum a b) where
  genericConstrIndex = case _ of
    Generic.Inl _ -> zero
    Generic.Inr x -> one + genericConstrIndex x

class ToContractError a where
  errorCodePrefix :: Proxy a -> String
  errorMessage :: a -> String

toContractError
  :: forall a rep
   . ToContractError a
  => Generic a rep
  => GenericConstrIndex rep
  => a
  -> ContractError
toContractError err =
  ContractError
    { errorCode:
        errorCodePrefix (Proxy :: Proxy a)
          <> Int.toStringAs Int.decimal (constrIndex err + one)
    , message:
        errorMessage err
    }

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
