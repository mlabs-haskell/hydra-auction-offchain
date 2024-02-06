module HydraAuctionOffchain.Contract.Validators.StandingBid
  ( mkStandingBidValidator
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Helpers (liftEitherShow)
import HydraAuctionOffchain.Lib.Script (reifyScript)
import Ply.Apply ((#!), (##))
import Ply.TypeList (Cons, Nil) as Ply
import Ply.Types (AsData, TypedScript, ValidatorRole)
import Ply.Types (toValidator) as Ply

foreign import standingBidValidator :: String

type StandingBidValidator =
  TypedScript
    ValidatorRole
    ( Ply.Cons (AsData CurrencySymbol)
        ( Ply.Cons (AsData AuctionTerms) Ply.Nil
        )
    )

mkStandingBidValidator :: CurrencySymbol -> AuctionTerms -> Contract Validator
mkStandingBidValidator auctionCs auctionTerms = do
  (reifiedValidator :: StandingBidValidator) <- reifyScript standingBidValidator
  liftEitherShow $ Ply.toValidator <$>
    reifiedValidator
      ## auctionCs
      #! auctionTerms
