module HydraAuctionOffchain.Contract.Scripts.AuctionMetadataValidator
  ( mkAuctionMetadataValidator
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (Validator)
import HydraAuctionOffchain.Contract.Scripts.Common (reifySimpleValidator)

foreign import auctionMetadataValidator :: String

mkAuctionMetadataValidator :: Contract Validator
mkAuctionMetadataValidator = reifySimpleValidator auctionMetadataValidator
