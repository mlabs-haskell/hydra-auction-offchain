module HydraAuctionOffchain.Contract.Scripts.AuctionValidators
  ( AuctionValidators(AuctionValidators)
  , mkAuctionValidators
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Typelevel.Undefined (undefined)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)

newtype AuctionValidators (a :: Type) = AuctionValidators
  { auctionEscrow :: a
  , bidderDeposit :: a
  , feeEscrow :: a
  , standingBid :: a
  }

derive instance Generic (AuctionValidators a) _
derive instance Newtype (AuctionValidators a) _
derive instance Eq a => Eq (AuctionValidators a)
derive instance Functor AuctionValidators

instance Show a => Show (AuctionValidators a) where
  show = genericShow

mkAuctionValidators :: AuctionTerms -> Contract (AuctionValidators Validator)
mkAuctionValidators = undefined
