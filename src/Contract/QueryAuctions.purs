module HydraAuctionOffchain.Contract.QueryAuctions
  ( queryAuctions
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTime)
import Contract.Transaction (TransactionOutput)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Contract.Value (valueOf) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Plutus.Types.Transaction (_output)
import Data.Array (mapMaybe) as Array
import Data.Lens ((^.))
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

-- | Queries all currently existing auctions at the auction metadata
-- | validator address, filtering out invalid entries.
queryAuctions :: AuctionFilters -> Contract (Array AuctionInfoExtended)
queryAuctions filters =
  case (unwrap filters).myRole of
    Just actorRole ->
      fromMaybe mempty <$> queryOwnAuctions actorRole
    Nothing -> do
      auctionMetadataAddr <- flip scriptHashAddress Nothing <<< validatorHash <$>
        mkAuctionMetadataValidator
      utxosAt auctionMetadataAddr
        <#> Array.mapMaybe getValidAuctionInfo
        <<< Map.toUnfoldable
  where
  getValidAuctionInfo :: Utxo -> Maybe AuctionInfoExtended
  getValidAuctionInfo (oref /\ txOutWithRefScript) = do
    auctionInfo@(AuctionInfo { auctionId, auctionTerms }) <- getInlineDatum txOut
    case validAuctionTerms auctionTerms && validAuctionId auctionId of
      true ->
        Just $ mkAuctionInfoExtended auctionInfo $ Just oref
      false ->
        Nothing
    where
    txOut :: TransactionOutput
    txOut = txOutWithRefScript ^. _output

    validAuctionTerms :: AuctionTerms -> Boolean
    validAuctionTerms auctionTerms =
      V.isValid $ validateAuctionTerms auctionTerms

    validAuctionId :: CurrencySymbol -> Boolean
    validAuctionId cs =
      Value.valueOf (unwrap txOut).amount cs auctionMetadataTokenName == one

queryOwnAuctions :: ActorRole -> Contract (Maybe (Array AuctionInfoExtended))
queryOwnAuctions actorRole =
  runMaybeT do
    pkh <- MaybeT ownPaymentPubKeyHash
    let personalOracle = mkPersonalOracle pkh
    let oracleAddr = scriptHashAddress (wrap personalOracle.nativeScriptHash) Nothing
    nowTime <- lift currentTime
    (lift $ utxosAt oracleAddr)
      <#> Array.mapMaybe (getValidAuctionInfoCached personalOracle.assetClass nowTime)
      <<< Map.toUnfoldable
  where
  getValidAuctionInfoCached :: AssetClass -> POSIXTime -> Utxo -> Maybe AuctionInfoExtended
  getValidAuctionInfoCached (AssetClass asset) nowTime (oref /\ txOutWithRefScript) =
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
    txOut :: TransactionOutput
    txOut = txOutWithRefScript ^. _output

    authTokenPresent :: Boolean
    authTokenPresent =
      Value.valueOf (unwrap txOut).amount asset.currencySymbol asset.tokenName
        == one

    validAuctionActor :: AuctionActor -> Maybe AuctionInfoExtended
    validAuctionActor (AuctionActor { auctionInfo, role })
      | role == actorRole
          && (nowTime < (unwrap (unwrap auctionInfo).auctionTerms).cleanup)
          && V.isValid (validateAuctionTerms (unwrap auctionInfo).auctionTerms) =
          Just auctionInfo
      | otherwise =
          Nothing
