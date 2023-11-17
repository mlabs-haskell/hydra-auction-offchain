module HydraAuctionOffchain.Contract.Scripts.AuctionEscrowValidator
  ( mkAuctionEscrowValidator
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Data.Typelevel.Undefined (undefined)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)

mkAuctionEscrowValidator :: AuctionTerms -> Contract Validator
mkAuctionEscrowValidator = undefined
