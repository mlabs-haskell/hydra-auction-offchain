module HydraAuctionOffchain.Contract.MintingPolicies.Auction
  ( auctionEscrowTokenName
  , auctionMetadataTokenName
  , mkAuctionMintingPolicy
  , standingBidTokenName
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy, ScriptHash)
import Contract.Transaction (TransactionInput)
import Contract.Value (TokenName)
import HydraAuctionOffchain.Helpers (liftEitherShow, tokenNameFromAsciiUnsafe)
import HydraAuctionOffchain.Lib.Script (reifyScript)
import Ply.Apply ((#!), (##))
import Ply.TypeList (Cons, Nil) as Ply
import Ply.Types (AsData, MintingPolicyRole, TypedScript)
import Ply.Types (toMintingPolicy) as Ply

foreign import auctionMintingPolicy :: String

type AuctionMintingPolicy =
  TypedScript
    MintingPolicyRole
    ( Ply.Cons (AsData ScriptHash)
        ( Ply.Cons (AsData TransactionInput) Ply.Nil
        )
    )

mkAuctionMintingPolicy :: ScriptHash -> TransactionInput -> Contract MintingPolicy
mkAuctionMintingPolicy auctionMetadataSh nonceOref = do
  (reifiedMp :: AuctionMintingPolicy) <- reifyScript auctionMintingPolicy
  liftEitherShow $ Ply.toMintingPolicy <$>
    reifiedMp
      ## auctionMetadataSh
      #! nonceOref

-- | Auction state token, identifying the true auction escrow.
auctionEscrowTokenName :: TokenName
auctionEscrowTokenName = tokenNameFromAsciiUnsafe "AUCTION"

-- | Auction metadata token, identifying the true auction metadata.
auctionMetadataTokenName :: TokenName
auctionMetadataTokenName = tokenNameFromAsciiUnsafe "AUCTION_METADATA"

-- | Standing bid token, identifying the true standing bid.
standingBidTokenName :: TokenName
standingBidTokenName = tokenNameFromAsciiUnsafe "STANDING_BID"
