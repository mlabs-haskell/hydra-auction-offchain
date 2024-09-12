module HydraAuctionOffchain.Contract.Validators.AuctionEscrow
  ( mkAuctionEscrowValidator
  ) where

import Prelude

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Value (CurrencySymbol)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import HydraAuctionOffchain.Contract.Types.Scripts (FeeEscrowScriptHash, StandingBidScriptHash)
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import auctionEscrowValidator :: String

mkAuctionEscrowValidator
  :: StandingBidScriptHash
  -> FeeEscrowScriptHash
  -> CurrencySymbol
  -> AuctionTerms
  -> Contract PlutusScript
mkAuctionEscrowValidator standingBidSh feeEscrowSh auctionCs auctionTerms =
  decodeApplyScript
    { scriptEnvelope: auctionEscrowValidator
    , scriptName: "AuctionEscrowValidator"
    , args:
        [ toData standingBidSh
        , toData feeEscrowSh
        , toData auctionCs
        , toData auctionTerms
        ]
    }
