module HydraAuctionOffchain.Contract.Validators.StandingBid
  ( mkStandingBidValidator
  ) where

import Prelude

import Cardano.ToData (toData)
import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import Contract.Value (CurrencySymbol)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Helpers (liftEitherShow)
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import standingBidValidator :: String

mkStandingBidValidator :: CurrencySymbol -> AuctionTerms -> Contract PlutusScript
mkStandingBidValidator auctionCs auctionTerms =
  decodeApplyScript
    { scriptEnvelope: standingBidValidator
    , scriptName: "StandingBidValidator"
    , args:
        [ toData auctionCs
        , toData auctionTerms
        ]
    }
