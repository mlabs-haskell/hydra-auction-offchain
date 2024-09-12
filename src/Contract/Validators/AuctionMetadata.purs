module HydraAuctionOffchain.Contract.Validators.AuctionMetadata
  ( mkAuctionMetadataValidator
  ) where

import Prelude

import Cardano.Types (PlutusScript)
import Contract.Monad (Contract)
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import auctionMetadataValidator :: String

mkAuctionMetadataValidator :: Contract PlutusScript
mkAuctionMetadataValidator =
  decodeApplyScript
    { scriptEnvelope: auctionMetadataValidator
    , scriptName: "AuctionMetadataValidator"
    , args: mempty
    }
