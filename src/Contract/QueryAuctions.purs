module HydraAuctionOffchain.Contract.QueryAuctions
  ( queryAuctions
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(Datum), OutputDatum(OutputDatum), fromData)
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Contract.Value (valueOf) as Value
import Control.Error.Util (bool)
import Data.Array (mapMaybe) as Array
import Data.Map (toUnfoldable) as Map
import Data.Validation.Semigroup (isValid) as V
import HydraAuctionOffchain.Contract.MintingPolicies (auctionMetadataTokenName)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfo(AuctionInfo)
  , AuctionTerms
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (mkAuctionMetadataValidator)

-- | Queries all currently existing auctions at the auction metadata validator address,
-- | filtering out invalid entries.
queryAuctions :: Contract (Array AuctionInfo)
queryAuctions = do
  auctionMetadataAddr <- flip scriptHashAddress Nothing <<< validatorHash <$>
    mkAuctionMetadataValidator
  utxos <- utxosAt auctionMetadataAddr
  let txOuts = Map.toUnfoldable utxos <#> _.output <<< unwrap <<< snd
  pure $ Array.mapMaybe getValidAuctionMetadata txOuts

getValidAuctionMetadata :: TransactionOutput -> Maybe AuctionInfo
getValidAuctionMetadata txOut = do
  auctionInfo@(AuctionInfo { auctionId, auctionTerms }) <- getAuctionInfo
  bool Nothing (Just auctionInfo)
    (validAuctionTerms auctionTerms && validAuctionId auctionId)
  where
  validAuctionTerms :: AuctionTerms -> Boolean
  validAuctionTerms auctionTerms = V.isValid $ validateAuctionTerms auctionTerms

  validAuctionId :: CurrencySymbol -> Boolean
  validAuctionId cs = Value.valueOf (unwrap txOut).amount cs auctionMetadataTokenName == one

  getAuctionInfo :: Maybe AuctionInfo
  getAuctionInfo =
    case (unwrap txOut).datum of
      OutputDatum (Datum plutusData) -> fromData plutusData
      _ -> Nothing
