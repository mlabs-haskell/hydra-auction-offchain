module HydraAuctionOffchain.Contract.Validators.AuctionMetadata
  ( mkAuctionMetadataValidator
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import HydraAuctionOffchain.Lib.Script (reifySimpleValidator)

foreign import auctionMetadataValidator :: String

mkAuctionMetadataValidator :: Contract Validator
mkAuctionMetadataValidator = reifySimpleValidator auctionMetadataValidator
