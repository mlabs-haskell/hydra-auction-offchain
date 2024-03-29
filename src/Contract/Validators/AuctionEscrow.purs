module HydraAuctionOffchain.Contract.Validators.AuctionEscrow
  ( mkAuctionEscrowValidator
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Scripts (FeeEscrowScriptHash, StandingBidScriptHash)
import HydraAuctionOffchain.Helpers (liftEitherShow)
import HydraAuctionOffchain.Lib.Script (reifyScript)
import Ply.Apply ((#!), (##))
import Ply.TypeList (Cons, Nil) as Ply
import Ply.Types (AsData, TypedScript, ValidatorRole)
import Ply.Types (toValidator) as Ply

foreign import auctionEscrowValidator :: String

type AuctionEscrowValidator =
  TypedScript
    ValidatorRole
    ( Ply.Cons (AsData StandingBidScriptHash)
        ( Ply.Cons (AsData FeeEscrowScriptHash)
            ( Ply.Cons (AsData CurrencySymbol)
                ( Ply.Cons (AsData AuctionTerms) Ply.Nil
                )
            )
        )
    )

mkAuctionEscrowValidator
  :: StandingBidScriptHash
  -> FeeEscrowScriptHash
  -> CurrencySymbol
  -> AuctionTerms
  -> Contract Validator
mkAuctionEscrowValidator standingBidSh feeEscrowSh auctionCs auctionTerms = do
  (reifiedValidator :: AuctionEscrowValidator) <- reifyScript auctionEscrowValidator
  liftEitherShow $ Ply.toValidator <$>
    reifiedValidator
      ## standingBidSh
      #! feeEscrowSh
      #! auctionCs
      #! auctionTerms
