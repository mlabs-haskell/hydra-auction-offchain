module HydraAuctionOffchain.Contract.QueryAuctions
  ( queryAuctions
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract)
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput)
import Contract.Value (CurrencySymbol)
import Contract.Value (valueOf) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Error.Util (bool)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (mapMaybe) as Array
import Data.Validation.Semigroup (isValid) as V
import HydraAuctionOffchain.Contract.MintingPolicies (auctionMetadataTokenName)
import HydraAuctionOffchain.Contract.PersonalOracle (mkPersonalOracle)
import HydraAuctionOffchain.Contract.Types
  ( ActorRole
  , AssetClass(AssetClass)
  , AuctionActor(AuctionActor)
  , AuctionFilters
  , AuctionInfo(AuctionInfo)
  , AuctionTerms
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (mkAuctionMetadataValidator)
import HydraAuctionOffchain.Helpers (getInlineDatum, getTxOutsAt)

-- | Queries all currently existing auctions at the auction metadata validator address,
-- | filtering out invalid entries.
queryAuctions :: AuctionFilters -> Contract (Array AuctionInfo)
queryAuctions filters =
  case (unwrap filters).myRole of
    Just actorRole ->
      fromMaybe mempty <$> queryOwnAuctions actorRole
    Nothing -> do
      auctionMetadataAddr <- flip scriptHashAddress Nothing <<< validatorHash <$>
        mkAuctionMetadataValidator
      getTxOutsAt auctionMetadataAddr
        <#> Array.mapMaybe getValidAuctionInfo
  where
  getValidAuctionInfo :: TransactionOutput -> Maybe AuctionInfo
  getValidAuctionInfo txOut = do
    auctionInfo@(AuctionInfo { auctionId, auctionTerms }) <- getInlineDatum txOut
    bool Nothing (Just auctionInfo)
      (validAuctionTerms auctionTerms && validAuctionId auctionId)
    where
    validAuctionTerms :: AuctionTerms -> Boolean
    validAuctionTerms auctionTerms =
      V.isValid $ validateAuctionTerms auctionTerms

    validAuctionId :: CurrencySymbol -> Boolean
    validAuctionId cs =
      Value.valueOf (unwrap txOut).amount cs auctionMetadataTokenName == one

queryOwnAuctions :: ActorRole -> Contract (Maybe (Array AuctionInfo))
queryOwnAuctions actorRole =
  runMaybeT do
    pkh <- MaybeT ownPaymentPubKeyHash
    let personalOracle = mkPersonalOracle pkh
    let oracleAddr = scriptHashAddress (wrap personalOracle.nativeScriptHash) Nothing
    (lift $ getTxOutsAt oracleAddr) <#>
      Array.mapMaybe (getValidAuctionInfo personalOracle.assetClass)
  where
  getValidAuctionInfo :: AssetClass -> TransactionOutput -> Maybe AuctionInfo
  getValidAuctionInfo (AssetClass asset) txOut
    | Value.valueOf (unwrap txOut).amount asset.currencySymbol asset.tokenName == one =
        case getInlineDatum txOut of
          Just (AuctionActor { auctionInfo, role }) | role == actorRole ->
            bool Nothing (Just auctionInfo)
              (V.isValid $ validateAuctionTerms (unwrap auctionInfo).auctionTerms)
          _ -> Nothing
    | otherwise = Nothing
