module HydraAuctionOffchain.Contract.QueryUtxo
  ( queryAuctionEscrowUtxo
  , queryBidderDepositUtxo
  , queryStandingBidUtxo
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (OutputDatum(OutputDatum), toData)
import Contract.Transaction (TransactionOutput(TransactionOutput))
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Contract.Value (valueOf) as Value
import Data.Array (find) as Array
import Data.Map (toUnfoldable) as Map
import HydraAuctionOffchain.Contract.MintingPolicies
  ( auctionEscrowTokenName
  , standingBidTokenName
  )
import HydraAuctionOffchain.Contract.Types
  ( AuctionEscrowState
  , BidderInfo
  , StandingBidState
  , Utxo
  , AuctionInfoRec
  )
import HydraAuctionOffchain.Helpers (getInlineDatum)

----------------------------------------------------------------------
-- AuctionEscrow

queryAuctionEscrowUtxo
  :: forall (r :: Row Type)
   . AuctionEscrowState
  -> Record (AuctionInfoRec r)
  -> Contract (Maybe Utxo)
queryAuctionEscrowUtxo escrowState auctionInfo =
  utxosAt auctionInfo.auctionEscrowAddr
    <#> Array.find (isValidAuctionEscrowUtxo <<< _.output <<< unwrap <<< snd)
    <<< Map.toUnfoldable
  where
  auctionCs :: CurrencySymbol
  auctionCs = auctionInfo.auctionId

  isValidAuctionEscrowUtxo :: TransactionOutput -> Boolean
  isValidAuctionEscrowUtxo (TransactionOutput txOut) =
    Value.valueOf txOut.amount auctionCs auctionEscrowTokenName == one
      && (txOut.datum == OutputDatum (wrap $ toData escrowState))

----------------------------------------------------------------------
-- StandingBid

queryStandingBidUtxo
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> Contract (Maybe (Utxo /\ StandingBidState))
queryStandingBidUtxo auctionInfo =
  utxosAt auctionInfo.standingBidAddr <#> \utxos -> do
    let getTxOut = _.output <<< unwrap <<< snd
    standingBidUtxo <- Array.find (hasStandingBidToken <<< getTxOut) $ Map.toUnfoldable utxos
    Tuple standingBidUtxo <$> getInlineDatum (getTxOut standingBidUtxo)
  where
  auctionCs :: CurrencySymbol
  auctionCs = auctionInfo.auctionId

  hasStandingBidToken :: TransactionOutput -> Boolean
  hasStandingBidToken txOut =
    Value.valueOf (unwrap txOut).amount auctionCs standingBidTokenName == one

----------------------------------------------------------------------
-- BidderDeposit

queryBidderDepositUtxo
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> BidderInfo
  -> Contract (Maybe Utxo)
queryBidderDepositUtxo auctionInfo bidderInfo =
  utxosAt auctionInfo.bidderDepositAddr
    <#> Array.find (eq (Just bidderInfo) <<< getInlineDatum <<< _.output <<< unwrap <<< snd)
    <<< Map.toUnfoldable
