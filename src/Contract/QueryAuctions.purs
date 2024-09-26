module HydraAuctionOffchain.Contract.QueryAuctions
  ( queryAuctions
  ) where

import Contract.Prelude

import Cardano.Types (Asset(Asset))
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Contract.Address (getNetworkId)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.Time (POSIXTime)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Contract.Value (valueOf) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (mapMaybe) as Array
import Data.Map (toUnfoldable) as Map
import Data.Newtype (modify)
import Data.Validation.Semigroup (isValid) as V
import HydraAuctionOffchain.Contract.MintingPolicies (auctionMetadataTokenName)
import HydraAuctionOffchain.Contract.PersonalOracle (mkPersonalOracle)
import HydraAuctionOffchain.Contract.Types
  ( ActorRole(ActorRoleBidder, ActorRoleSeller)
  , AssetClass(AssetClass)
  , AuctionActor(AuctionActor)
  , AuctionFilters
  , AuctionInfo(AuctionInfo)
  , AuctionInfoExtended
  , AuctionTerms
  , Utxo
  , mkAuctionInfoExtended
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (mkAuctionMetadataValidator)
import HydraAuctionOffchain.Helpers (getInlineDatum)
import HydraAuctionOffchain.Lib.Cardano.Address (scriptHashAddress)

-- | Queries all currently existing auctions at the auction metadata
-- | validator address, filtering out invalid entries.
queryAuctions :: AuctionFilters -> Contract (Array AuctionInfoExtended)
queryAuctions filters =
  case (unwrap filters).myRole of
    Just actorRole ->
      fromMaybe mempty <$> queryOwnAuctions actorRole
    Nothing -> do
      network <- getNetworkId
      auctionMetadataAddr <- scriptHashAddress network <<< PlutusScript.hash <$>
        mkAuctionMetadataValidator
      utxosAt auctionMetadataAddr
        <#> Array.mapMaybe getValidAuctionInfo
        <<< Map.toUnfoldable
  where
  getValidAuctionInfo :: Utxo -> Maybe AuctionInfoExtended
  getValidAuctionInfo (oref /\ txOut) = do
    auctionInfo@(AuctionInfo { auctionId, auctionTerms }) <- getInlineDatum txOut
    case validAuctionTerms auctionTerms && validAuctionId auctionId of
      true ->
        Just $ mkAuctionInfoExtended auctionInfo $ Just oref
      false ->
        Nothing
    where
    validAuctionTerms :: AuctionTerms -> Boolean
    validAuctionTerms auctionTerms =
      V.isValid $ validateAuctionTerms auctionTerms

    validAuctionId :: CurrencySymbol -> Boolean
    validAuctionId cs =
      Value.valueOf (Asset cs auctionMetadataTokenName) (unwrap txOut).amount
        == BigNum.one

queryOwnAuctions :: ActorRole -> Contract (Maybe (Array AuctionInfoExtended))
queryOwnAuctions actorRole =
  runMaybeT do
    network <- lift getNetworkId
    pkh <- MaybeT ownPaymentPubKeyHash
    let personalOracle = mkPersonalOracle network pkh
    nowTime <- lift currentTime
    (lift $ utxosAt personalOracle.address)
      <#> Array.mapMaybe (getValidAuctionInfoCached personalOracle.assetClass nowTime)
      <<< Map.toUnfoldable
  where
  getValidAuctionInfoCached :: AssetClass -> POSIXTime -> Utxo -> Maybe AuctionInfoExtended
  getValidAuctionInfoCached (AssetClass asset) nowTime (oref /\ txOut) =
    case authTokenPresent of
      false ->
        Nothing
      true ->
        (validAuctionActor =<< getInlineDatum txOut) <#> \auctionInfo ->
          case actorRole of
            ActorRoleBidder ->
              auctionInfo
            ActorRoleSeller ->
              auctionInfo # modify
                ( _
                    { metadataOref = Just $ wrap
                        { transactionId: (unwrap oref).transactionId
                        , index: zero
                        }
                    }
                )
    where
    authTokenPresent :: Boolean
    authTokenPresent =
      Value.valueOf (Asset asset.currencySymbol asset.tokenName) (unwrap txOut).amount
        == BigNum.one

    validAuctionActor :: AuctionActor -> Maybe AuctionInfoExtended
    validAuctionActor (AuctionActor { auctionInfo, role })
      | role == actorRole
          && (nowTime < (unwrap (unwrap auctionInfo).auctionTerms).cleanup)
          && V.isValid (validateAuctionTerms (unwrap auctionInfo).auctionTerms) =
          Just auctionInfo
      | otherwise =
          Nothing
