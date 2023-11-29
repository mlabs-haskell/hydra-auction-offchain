module HydraAuctionOffchain.Contract.Validators.AuctionEscrow
  ( mkAuctionEscrowValidator
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import Undefined (undefined)

mkAuctionEscrowValidator :: AuctionTerms -> Contract Validator
mkAuctionEscrowValidator = undefined
