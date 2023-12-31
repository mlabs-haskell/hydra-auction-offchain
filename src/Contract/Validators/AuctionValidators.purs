module HydraAuctionOffchain.Contract.Validators.AuctionValidators
  ( AuctionValidators(AuctionValidators)
  , mkAuctionValidators
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Validators.AlwaysSucceeds
  ( mkAlwaysSucceedsValidator
  )

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
mkAuctionValidators _ =
  mkAlwaysSucceedsValidator <#> \alwaysSucceedsStub -> wrap
    { auctionEscrow: alwaysSucceedsStub
    , bidderDeposit: alwaysSucceedsStub
    , feeEscrow: alwaysSucceedsStub
    , standingBid: alwaysSucceedsStub
    }
