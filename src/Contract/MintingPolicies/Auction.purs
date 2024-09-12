module HydraAuctionOffchain.Contract.MintingPolicies.Auction
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , mkAuctionMintingPolicy
  , standingBidTokenName
  ) where

import Prelude

import Cardano.ToData (toData)
import Cardano.Types (AssetName, PlutusScript, ScriptHash)
import Contract.Monad (Contract)
import Contract.Transaction (TransactionInput)
import HydraAuctionOffchain.Helpers (tokenNameFromAsciiUnsafe)
import HydraAuctionOffchain.Lib.Script (decodeApplyScript)

foreign import auctionMintingPolicy :: String

mkAuctionMintingPolicy :: ScriptHash -> TransactionInput -> Contract PlutusScript
mkAuctionMintingPolicy auctionMetadataSh nonceOref =
  decodeApplyScript
    { scriptEnvelope: auctionMintingPolicy
    , scriptName: "AuctionMintingPolicy"
    , args:
        [ toData auctionMetadataSh
        , toData nonceOref
        ]
    }

-- | Auction state token, identifying the true auction escrow.
auctionEscrowTokenName :: AssetName
auctionEscrowTokenName = tokenNameFromAsciiUnsafe "AUCTION"

-- | Auction metadata token, identifying the true auction metadata.
auctionMetadataTokenName :: AssetName
auctionMetadataTokenName = tokenNameFromAsciiUnsafe "AUCTION_METADATA"

-- | Standing bid token, identifying the true standing bid.
standingBidTokenName :: AssetName
standingBidTokenName = tokenNameFromAsciiUnsafe "STANDING_BID"
