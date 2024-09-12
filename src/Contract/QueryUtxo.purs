module HydraAuctionOffchain.Contract.QueryUtxo
  ( findStandingBidUtxo
  , isStandingBidUtxo
  , queryAuctionEscrowUtxo
  , queryBidderDepositUtxo
  , queryStandingBidUtxo
  ) where

import Contract.Prelude

import Cardano.Plutus.Types.Address (fromCardano, toCardano) as Plutus.Address
import Cardano.Types (Asset(Asset))
import Cardano.Types.BigNum (one) as BigNum
import Contract.Address (getNetworkId)
import Contract.Monad (Contract)
import Contract.PlutusData (OutputDatum(OutputDatum), toData)
import Contract.Transaction (TransactionOutput(TransactionOutput))
import Contract.Utxos (UtxoMap, utxosAt)
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
queryAuctionEscrowUtxo escrowState auctionInfo = do
  network <- getNetworkId
  case Plutus.Address.toCardano network auctionInfo.auctionEscrowAddr of
    Just auctionEscrowAddr ->
      utxosAt auctionEscrowAddr
        <#> Array.find (isValidAuctionEscrowUtxo <<< snd)
        <<< Map.toUnfoldable
    Nothing ->
      pure Nothing
  where
  auctionCs :: CurrencySymbol
  auctionCs = auctionInfo.auctionId

  isValidAuctionEscrowUtxo :: TransactionOutput -> Boolean
  isValidAuctionEscrowUtxo (TransactionOutput txOut) =
    (Value.valueOf (Asset auctionCs auctionEscrowTokenName) txOut.amount == BigNum.one)
      && (txOut.datum == Just (OutputDatum $ toData escrowState))

----------------------------------------------------------------------
-- StandingBid

queryStandingBidUtxo
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> Contract (Maybe (Utxo /\ StandingBidState))
queryStandingBidUtxo auctionInfo = do
  network <- getNetworkId
  case Plutus.Address.toCardano network auctionInfo.standingBidAddr of
    Just standingBidAddr ->
      findStandingBidUtxo auctionInfo <$> utxosAt standingBidAddr
    Nothing ->
      pure Nothing

findStandingBidUtxo
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> UtxoMap
  -> Maybe (Utxo /\ StandingBidState)
findStandingBidUtxo auctionInfo utxos = do
  standingBidUtxo <-
    Array.find (isStandingBidUtxo auctionInfo <<< snd)
      (Map.toUnfoldable utxos)
  Tuple standingBidUtxo <$> getInlineDatum (snd standingBidUtxo)

isStandingBidUtxo
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> TransactionOutput
  -> Boolean
isStandingBidUtxo auctionInfo (TransactionOutput txOut) =
  Plutus.Address.fromCardano txOut.address == Just auctionInfo.standingBidAddr
    &&
      ( Value.valueOf (Asset auctionInfo.auctionId standingBidTokenName) txOut.amount ==
          BigNum.one
      )

----------------------------------------------------------------------
-- BidderDeposit

queryBidderDepositUtxo
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> BidderInfo
  -> Contract (Maybe Utxo)
queryBidderDepositUtxo auctionInfo bidderInfo = do
  network <- getNetworkId
  case Plutus.Address.toCardano network auctionInfo.bidderDepositAddr of
    Just bidderDepositAddr ->
      utxosAt bidderDepositAddr
        <#> Array.find (eq (Just bidderInfo) <<< getInlineDatum <<< snd)
        <<< Map.toUnfoldable
    Nothing ->
      pure Nothing
