module HydraAuctionOffchain.Contract.DiscoverBidders
  ( BidderInfoCandidate(BidderInfoCandidate)
  , discoverBidders
  ) where

import Contract.Prelude hiding (oneOf)

import Cardano.Plutus.Types.Address (toCardano) as Plutus.Address
import Cardano.Types (BigNum, NetworkId, TransactionOutput)
import Cardano.Types.Value (valueToCoin)
import Contract.Address (getNetworkId)
import Contract.Monad (Contract)
import Contract.Utxos (utxosAt)
import Data.Array (mapMaybe) as Array
import Data.Codec.Argonaut (JsonCodec, boolean, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Map (toUnfoldable) as Map
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Codec (bigNumCodec)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended(AuctionInfoExtended)
  , BidderInfo
  , bidderInfoCodec
  )
import HydraAuctionOffchain.Helpers (getInlineDatum)
import HydraAuctionOffchain.Lib.Codec (class HasJson)

newtype BidderInfoCandidate = BidderInfoCandidate
  { bidderInfo :: BidderInfo
  , depositAmount :: BigNum
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
      , depositAmount: bigNumCodec
      , isValid: CA.boolean
      }

discoverBidders :: AuctionInfoExtended -> Contract (Array BidderInfoCandidate)
discoverBidders (AuctionInfoExtended auctionInfo) = do
  network <- getNetworkId
  case Plutus.Address.toCardano network auctionInfo.bidderDepositAddr of
    Just bidderDepositAddr -> do
      utxos <- utxosAt bidderDepositAddr
      let txOuts = snd <$> Map.toUnfoldable utxos
      pure $ Array.mapMaybe getBidderInfoCandidate txOuts
    Nothing ->
      pure mempty -- FIXME: throw error
  where
  getBidderInfoCandidate :: TransactionOutput -> Maybe BidderInfoCandidate
  getBidderInfoCandidate txOut =
    getInlineDatum txOut <#> \bidderInfo -> wrap
      { bidderInfo
      , depositAmount
      , isValid: depositAmount >= (unwrap auctionInfo.auctionTerms).minDepositAmount
      }
    where
    depositAmount :: BigNum
    depositAmount = unwrap $ valueToCoin (unwrap txOut).amount
