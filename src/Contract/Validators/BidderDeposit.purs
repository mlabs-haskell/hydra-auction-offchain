module HydraAuctionOffchain.Contract.Validators.BidderDeposit
  ( mkBidderDepositValidator
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Contract.Value (CurrencySymbol)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Scripts
  ( AuctionEscrowScriptHash
  , StandingBidScriptHash
  )
import HydraAuctionOffchain.Helpers (liftEitherShow)
import HydraAuctionOffchain.Lib.Script (reifyScript)
import Ply.Apply ((#!), (##))
import Ply.TypeList (Cons, Nil) as Ply
import Ply.Types (AsData, TypedScript, ValidatorRole)
import Ply.Types (toValidator) as Ply

foreign import bidderDepositValidator :: String

type BidderDepositValidator =
  TypedScript
    ValidatorRole
    ( Ply.Cons (AsData StandingBidScriptHash)
        ( Ply.Cons (AsData AuctionEscrowScriptHash)
            ( Ply.Cons (AsData CurrencySymbol)
                ( Ply.Cons (AsData AuctionTerms) Ply.Nil
                )
            )
        )
    )

mkBidderDepositValidator
  :: StandingBidScriptHash
  -> AuctionEscrowScriptHash
  -> CurrencySymbol
  -> AuctionTerms
  -> Contract Validator
mkBidderDepositValidator standingBidSh auctionEscrowSh auctionCs auctionTerms = do
  (reifiedValidator :: BidderDepositValidator) <- reifyScript bidderDepositValidator
  liftEitherShow $ Ply.toValidator <$>
    reifiedValidator
      ## standingBidSh
      #! auctionEscrowSh
      #! auctionCs
      #! auctionTerms
