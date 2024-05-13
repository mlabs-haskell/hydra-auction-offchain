module HydraAuctionOffchain.Contract.Validators.AuctionValidators
  ( AuctionValidators(AuctionValidators)
  , MkAuctionValidatorsError
      ( MkStandingBidValidatorError
      , MkAuctionEscrowValidatorError
      , MkBidderDepositValidatorError
      )
  , mkAuctionValidators
  , mkAuctionValidatorsErrorCodec
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator, validatorHash)
import Contract.Value (CurrencySymbol)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Scripts
  ( AuctionEscrowScriptHash(AuctionEscrowScriptHash)
  , FeeEscrowScriptHash(FeeEscrowScriptHash)
  , StandingBidScriptHash(StandingBidScriptHash)
  )
import HydraAuctionOffchain.Contract.Validators.AlwaysSucceeds (mkAlwaysSucceedsValidator)
import HydraAuctionOffchain.Contract.Validators.AuctionEscrow (mkAuctionEscrowValidator)
import HydraAuctionOffchain.Contract.Validators.BidderDeposit (mkBidderDepositValidator)
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
  | MkBidderDepositValidatorError

derive instance Generic MkAuctionValidatorsError _
derive instance Eq MkAuctionValidatorsError

instance Show MkAuctionValidatorsError where
  show = genericShow

mkAuctionValidatorsErrorCodec :: CA.JsonCodec MkAuctionValidatorsError
mkAuctionValidatorsErrorCodec =
  CAG.nullarySum "MkAuctionValidatorsError"

mkAuctionValidators
  :: CurrencySymbol
  -> AuctionTerms
  -> ExceptT MkAuctionValidatorsError Contract (AuctionValidators Validator)
mkAuctionValidators auctionCs auctionTerms = do
  feeEscrow <- lift mkAlwaysSucceedsValidator
  standingBid <- mkStandingBidValidator auctionCs auctionTerms !* MkStandingBidValidatorError
  let
    standingBidSh = StandingBidScriptHash $ unwrap $ validatorHash standingBid
    mkAuctionEscrow = mkAuctionEscrowValidator
      standingBidSh
      (FeeEscrowScriptHash $ unwrap $ validatorHash feeEscrow)
      auctionCs
      auctionTerms
  auctionEscrow <- mkAuctionEscrow !* MkAuctionEscrowValidatorError
  let
    mkBidderDeposit = mkBidderDepositValidator
      standingBidSh
      (AuctionEscrowScriptHash $ unwrap $ validatorHash auctionEscrow)
      auctionCs
      auctionTerms
  bidderDeposit <- mkBidderDeposit !* MkBidderDepositValidatorError
  pure $ AuctionValidators
    { auctionEscrow
    , bidderDeposit
    , feeEscrow
    , standingBid
    }
