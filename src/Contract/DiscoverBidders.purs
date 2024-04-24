module HydraAuctionOffchain.Contract.DiscoverBidders
  ( BidderInfoCandidate(BidderInfoCandidate)
  , discoverBidders
  ) where

import Contract.Prelude hiding (oneOf)

import Contract.Config (NetworkId)
import Contract.Monad (Contract)
import Contract.Transaction (TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (valueToCoin') as Value
import Data.Array (mapMaybe) as Array
import Data.Codec.Argonaut (JsonCodec, boolean, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Map (toUnfoldable) as Map
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (bigIntCodec)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended(AuctionInfoExtended)
  , BidderInfo
  , bidderInfoCodec
  )
import HydraAuctionOffchain.Helpers (getInlineDatum)
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import JS.BigInt (BigInt)

newtype BidderInfoCandidate = BidderInfoCandidate
  { bidderInfo :: BidderInfo
  , depositAmount :: BigInt
  , isValid :: Boolean
  }

derive instance Generic BidderInfoCandidate _
derive instance Newtype BidderInfoCandidate _
derive instance Eq BidderInfoCandidate

instance Show BidderInfoCandidate where
  show = genericShow

instance HasJson BidderInfoCandidate NetworkId where
  jsonCodec network = const (bidderInfoCandidateCodec network)

bidderInfoCandidateCodec :: NetworkId -> CA.JsonCodec BidderInfoCandidate
bidderInfoCandidateCodec network =
  wrapIso BidderInfoCandidate $ CA.object "BidderInfoCandidate" $
    CAR.record
      { bidderInfo: bidderInfoCodec network
      , depositAmount: bigIntCodec
      , isValid: CA.boolean
      }

discoverBidders :: AuctionInfoExtended -> Contract (Array BidderInfoCandidate)
discoverBidders (AuctionInfoExtended auctionInfo) = do
  utxos <- utxosAt auctionInfo.bidderDepositAddr
  let txOuts = Map.toUnfoldable utxos <#> _.output <<< unwrap <<< snd
  pure $ Array.mapMaybe getBidderInfoCandidate txOuts
  where
  getBidderInfoCandidate :: TransactionOutput -> Maybe BidderInfoCandidate
  getBidderInfoCandidate txOut =
    getInlineDatum txOut <#> \bidderInfo -> wrap
      { bidderInfo
      , depositAmount
      , isValid: depositAmount >= (unwrap auctionInfo.auctionTerms).minDepositAmount
      }
    where
    depositAmount :: BigInt
    depositAmount = Value.valueToCoin' (unwrap txOut).amount
