module HydraAuctionOffchain.Contract.MintingPolicies.Auction
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , mkAuctionMintingPolicy
  , standingBidTokenName
  ) where

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy)
import Contract.Transaction (TransactionInput)
import Contract.Value (TokenName)
import HydraAuctionOffchain.Contract.MintingPolicies.AlwaysMints (mkAlwaysMintsPolicy)
import HydraAuctionOffchain.Helpers (tokenNameFromAsciiUnsafe)
import Undefined (undefined)

mkAuctionMintingPolicy :: TransactionInput -> Contract MintingPolicy
mkAuctionMintingPolicy _nonce = mkAlwaysMintsPolicy

-- | Auction state token, identifying the true auction escrow.
auctionEscrowTokenName :: TokenName
auctionEscrowTokenName = tokenNameFromAsciiUnsafe "AUCTION"

-- | Auction metadata token, identifying the true auction metadata.
auctionMetadataTokenName :: TokenName
auctionMetadataTokenName = tokenNameFromAsciiUnsafe "AUCTION_METADATA"

-- | Standing bid token, identifying the true standing bid.
standingBidTokenName :: TokenName
standingBidTokenName = tokenNameFromAsciiUnsafe "STANDING_BID"
