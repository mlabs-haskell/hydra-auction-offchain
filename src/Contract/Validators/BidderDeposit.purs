module HydraAuctionOffchain.Contract.Validators.BidderDeposit
  ( mkBidderDepositValidator
  ) where

import Cardano.ToData (toData)
import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import Contract.Value (CurrencySymbol)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Scripts
  ( AuctionEscrowScriptHash
  , StandingBidScriptHash
  )
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import bidderDepositValidator :: String

mkBidderDepositValidator
  :: StandingBidScriptHash
  -> AuctionEscrowScriptHash
  -> CurrencySymbol
  -> AuctionTerms
  -> Contract PlutusScript
mkBidderDepositValidator standingBidSh auctionEscrowSh auctionCs auctionTerms =
  decodeApplyScript
    { scriptEnvelope: bidderDepositValidator
    , scriptName: "BidderDepositValidator"
    , args:
        [ toData standingBidSh
        , toData auctionEscrowSh
        , toData auctionCs
        , toData auctionTerms
        ]
    }
