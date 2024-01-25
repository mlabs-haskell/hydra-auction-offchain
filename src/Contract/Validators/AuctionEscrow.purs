module HydraAuctionOffchain.Contract.Validators.AuctionEscrow
  ( mkAuctionEscrowValidator
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (ScriptHash, Validator)
import Contract.Value (CurrencySymbol)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Validators.Common (reifyValidator)
import HydraAuctionOffchain.Helpers (liftEitherShow)
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
