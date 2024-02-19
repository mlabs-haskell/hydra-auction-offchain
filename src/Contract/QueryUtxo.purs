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
  , AuctionInfo(AuctionInfo)
  , BidderInfo
  , StandingBidState
  , Utxo
  )
import HydraAuctionOffchain.Helpers (getInlineDatum)

----------------------------------------------------------------------
-- AuctionEscrow

queryAuctionEscrowUtxo :: AuctionEscrowState -> AuctionInfo -> Contract (Maybe Utxo)
queryAuctionEscrowUtxo escrowState (AuctionInfo auctionInfo) =
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

queryStandingBidUtxo :: AuctionInfo -> Contract (Maybe (Utxo /\ StandingBidState))
queryStandingBidUtxo (AuctionInfo auctionInfo) =
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

queryBidderDepositUtxo :: AuctionInfo -> BidderInfo -> Contract (Maybe Utxo)
queryBidderDepositUtxo (AuctionInfo auctionInfo) bidderInfo =
  utxosAt auctionInfo.bidderDepositAddr
    <#> Array.find (eq (Just bidderInfo) <<< getInlineDatum <<< _.output <<< unwrap <<< snd)
    <<< Map.toUnfoldable
