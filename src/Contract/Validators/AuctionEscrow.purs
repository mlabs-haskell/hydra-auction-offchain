module HydraAuctionOffchain.Contract.Validators.AuctionEscrow
  ( MkAuctionEscrowValidatorError
      ( StandingBidScriptHashError
      , FeeEscrowScriptHashError
      , AuctionEscrowValidatorReificationError
      )
  , mkAuctionEscrowValidator
  , mkAuctionEscrowValidatorFromAuctionInfo
  ) where

import Prelude

import Contract.Address (addressPaymentValidatorHash)
import Contract.Monad (Contract)
import Contract.Scripts (ScriptHash, Validator)
import Contract.Value (CurrencySymbol)
import Control.Error.Util ((??))
import Control.Monad.Except (ExceptT)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionInfo (AuctionInfo(AuctionInfo))
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Validators.Common (reifyValidator)
import HydraAuctionOffchain.Helpers (liftEitherShow, (!*))
import Ply.Apply ((#!), (##))
import Ply.TypeList (Cons, Nil) as Ply
import Ply.Types (AsData, TypedScript, ValidatorRole)
import Ply.Types (toValidator) as Ply

foreign import auctionEscrowValidator :: String

type AuctionEscrowValidator =
  TypedScript
    ValidatorRole
    ( Ply.Cons (AsData ScriptHash)
        ( Ply.Cons (AsData ScriptHash)
            ( Ply.Cons (AsData CurrencySymbol)
                ( Ply.Cons (AsData AuctionTerms) Ply.Nil
                )
            )
        )
    )

data MkAuctionEscrowValidatorError
  = StandingBidScriptHashError
  | FeeEscrowScriptHashError
  | AuctionEscrowValidatorReificationError

derive instance Generic MkAuctionEscrowValidatorError _
derive instance Eq MkAuctionEscrowValidatorError

instance Show MkAuctionEscrowValidatorError where
  show = genericShow

mkAuctionEscrowValidatorFromAuctionInfo
  :: AuctionInfo
  -> ExceptT MkAuctionEscrowValidatorError Contract Validator
mkAuctionEscrowValidatorFromAuctionInfo (AuctionInfo auctionInfo) = do
  standingBidSh <-
    unwrap <$> addressPaymentValidatorHash auctionInfo.standingBidAddr
      ?? StandingBidScriptHashError
  feeEscrowSh <-
    unwrap <$> addressPaymentValidatorHash auctionInfo.feeEscrowAddr
      ?? FeeEscrowScriptHashError
  let
    mkAuctionEscrow =
      mkAuctionEscrowValidator standingBidSh feeEscrowSh auctionInfo.auctionId
        auctionInfo.auctionTerms
  mkAuctionEscrow !* AuctionEscrowValidatorReificationError

mkAuctionEscrowValidator
  :: ScriptHash
  -> ScriptHash
  -> CurrencySymbol
  -> AuctionTerms
  -> Contract Validator
mkAuctionEscrowValidator standingBidSh feeEscrowSh auctionCs auctionTerms = do
  (reifiedValidator :: AuctionEscrowValidator) <- reifyValidator auctionEscrowValidator
  liftEitherShow $ Ply.toValidator <$>
    reifiedValidator
      ## standingBidSh
      #! feeEscrowSh
      #! auctionCs
      #! auctionTerms
