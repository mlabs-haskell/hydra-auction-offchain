module HydraAuctionOffchain.Contract.Validators.AuctionValidators
  ( AuctionValidators(AuctionValidators)
  , MkAuctionValidatorsError(MkStandingBidValidatorError, MkAuctionEscrowValidatorError)
  , mkAuctionValidators
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator, validatorHash)
import Contract.Value (CurrencySymbol)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Validators.AlwaysSucceeds (mkAlwaysSucceedsValidator)
import HydraAuctionOffchain.Contract.Validators.AuctionEscrow (mkAuctionEscrowValidator)
import HydraAuctionOffchain.Contract.Validators.StandingBid (mkStandingBidValidator)
import HydraAuctionOffchain.Helpers ((!*))

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

data MkAuctionValidatorsError
  = MkStandingBidValidatorError
  | MkAuctionEscrowValidatorError

derive instance Generic MkAuctionValidatorsError _
derive instance Eq MkAuctionValidatorsError

instance Show MkAuctionValidatorsError where
  show = genericShow

mkAuctionValidators
  :: CurrencySymbol
  -> AuctionTerms
  -> ExceptT MkAuctionValidatorsError Contract (AuctionValidators Validator)
mkAuctionValidators auctionCs auctionTerms = do
  bidderDeposit <- lift mkAlwaysSucceedsValidator
  feeEscrow <- lift mkAlwaysSucceedsValidator
  standingBid <- mkStandingBidValidator auctionCs auctionTerms !* MkStandingBidValidatorError
  let
    mkAuctionEscrow = mkAuctionEscrowValidator (unwrap $ validatorHash standingBid)
      (unwrap $ validatorHash feeEscrow)
      auctionCs
      auctionTerms
  auctionEscrow <- mkAuctionEscrow !* MkAuctionEscrowValidatorError
  pure $ AuctionValidators
    { auctionEscrow
    , bidderDeposit
    , feeEscrow
    , standingBid
    }
