module HydraAuctionOffchain.Contract.Scripts.AuctionMintingPolicy
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , mkAuctionMintingPolicy
  , standingBidTokenName
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol, TokenName)
import Data.Typelevel.Undefined (undefined)
import HydraAuctionOffchain.Helpers (tokenNameFromAsciiUnsafe)

mkAuctionMintingPolicy :: TransactionInput -> Contract MintingPolicy
mkAuctionMintingPolicy nonce = undefined

-- | Auction state token, identifying the true auction escrow.
auctionEscrowTokenName :: TokenName
auctionEscrowTokenName = tokenNameFromAsciiUnsafe "AUCTION"

-- | Auction metadata token, identifying the true auction metadata.
auctionMetadataTokenName :: TokenName
auctionMetadataTokenName = tokenNameFromAsciiUnsafe "AUCTION_METADATA"

-- | Standing bid token, identifying the true standing bid.
standingBidTokenName :: TokenName
standingBidTokenName = tokenNameFromAsciiUnsafe "STANDING_BID"
