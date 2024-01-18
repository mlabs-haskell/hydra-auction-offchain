module HydraAuctionOffchain.Contract.DiscoverBidders
  ( BidderInfoCandidate(BidderInfoCandidate)
  , discoverBidders
  ) where

import Contract.Prelude hiding (oneOf)

import Contract.Monad (Contract)
import Contract.PlutusData (Datum(Datum), OutputDatum(OutputDatum), fromData)
import Contract.Transaction (TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (valueToCoin') as Value
import Data.Array (mapMaybe) as Array
import Data.BigInt (BigInt)
import Data.Codec.Argonaut (JsonCodec, boolean, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Map (toUnfoldable) as Map
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (class HasJson, bigIntCodec)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfo(AuctionInfo)
  , BidderInfo
  , bidderInfoCodec
  )

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

instance HasJson BidderInfoCandidate where
  jsonCodec = const bidderInfoCandidateCodec

bidderInfoCandidateCodec :: CA.JsonCodec BidderInfoCandidate
bidderInfoCandidateCodec =
  wrapIso BidderInfoCandidate $ CA.object "BidderInfoCandidate" $
    CAR.record
      { bidderInfo: bidderInfoCodec
      , depositAmount: bigIntCodec
      , isValid: CA.boolean
      }

discoverBidders :: AuctionInfo -> Contract (Array BidderInfoCandidate)
discoverBidders (AuctionInfo auctionInfo) = do
  utxos <- utxosAt auctionInfo.bidderDepositAddr
  let txOuts = Map.toUnfoldable utxos <#> _.output <<< unwrap <<< snd
  pure $ Array.mapMaybe getBidderInfoCandidate txOuts
  where
  getBidderInfoCandidate :: TransactionOutput -> Maybe BidderInfoCandidate
  getBidderInfoCandidate txOut =
    getBidderInfo <#> \bidderInfo -> wrap
      { bidderInfo
      , depositAmount
      , isValid: depositAmount >= (unwrap auctionInfo.auctionTerms).minDepositAmount
      }
    where
    depositAmount :: BigInt
    depositAmount = Value.valueToCoin' (unwrap txOut).amount

    getBidderInfo :: Maybe BidderInfo
    getBidderInfo =
      case (unwrap txOut).datum of
        OutputDatum (Datum plutusData) -> fromData plutusData
        _ -> Nothing
