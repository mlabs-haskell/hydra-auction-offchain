module HydraAuctionOffchain.Contract.Types.ContractOutput
  ( ContractOutput(ContractOutputError, ContractOutputResult)
  , contractOutputCodec
  , mkContractOutput
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right), either)
import Data.Generic.Rep (class Generic)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Variant (inj, match) as Variant
import HydraAuctionOffchain.Contract.Types.ContractError
  ( class GenericConstrIndex
  , class ToContractError
  , ContractError
  , contractErrorCodec
  , toContractError
  )
import HydraAuctionOffchain.Lib.Codec (class HasJson, jsonCodec)
import Type.Proxy (Proxy(Proxy))

data ContractOutput (a :: Type)
  = ContractOutputError ContractError
  | ContractOutputResult a

derive instance Generic (ContractOutput a) _
derive instance Eq a => Eq (ContractOutput a)

instance Show a => Show (ContractOutput a) where
  show = genericShow

instance HasJson a anyParams => HasJson (ContractOutput a) anyParams where
  jsonCodec params =
    const (contractOutputCodec $ jsonCodec params Proxy)

contractOutputCodec :: forall (a :: Type). CA.JsonCodec a -> CA.JsonCodec (ContractOutput a)
contractOutputCodec contractResultCodec =
  dimap toVariant fromVariant $ CAV.variantMatch
    { error: Right contractErrorCodec
    , result: Right contractResultCodec
    }
  where
  toVariant = case _ of
    ContractOutputError err ->
      Variant.inj (Proxy :: Proxy "error") err
    ContractOutputResult res ->
      Variant.inj (Proxy :: Proxy "result") res

  fromVariant = Variant.match
    { error: ContractOutputError
    , result: ContractOutputResult
    }

mkContractOutput
  :: forall e rep m a b
   . ToContractError e
  => Generic e rep
  => GenericConstrIndex rep
  => Functor m
  => (a -> b)
  -> ExceptT e m a
  -> m (ContractOutput b)
mkContractOutput f contract =
  runExceptT contract <#>
    either (ContractOutputError <<< toContractError) (ContractOutputResult <<< f)
