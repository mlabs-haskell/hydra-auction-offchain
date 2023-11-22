module HydraAuctionOffchain.Contract.Scripts.AuctionMetadataValidator
  ( mkAuctionMetadataValidator
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import Data.Typelevel.Undefined (undefined)

mkAuctionMetadataValidator :: Contract Validator
mkAuctionMetadataValidator = undefined
