module HydraAuctionOffchain.Contract.Types.Plutus.Extra.AssetClass
  ( AssetClass(AssetClass)
  , assetToTuple
  , assetToValue
  , mkAssetClass
  ) where

import Prelude

import Contract.PlutusData (class FromData, class ToData, PlutusData(List), fromData, toData)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (singleton) as Value
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))

newtype AssetClass = AssetClass
  { currencySymbol :: CurrencySymbol
  , tokenName :: TokenName
  }

derive instance Generic AssetClass _
derive instance Newtype AssetClass _
derive instance Eq AssetClass

instance Show AssetClass where
  show = genericShow

instance ToData AssetClass where
  toData (AssetClass rec) =
    List [ toData rec.currencySymbol, toData rec.tokenName ]

instance FromData AssetClass where
  fromData (List [ cs, tn ]) =
    wrap <$> ({ currencySymbol: _, tokenName: _ } <$> fromData cs <*> fromData tn)
  fromData _ = Nothing

mkAssetClass :: CurrencySymbol -> TokenName -> AssetClass
mkAssetClass currencySymbol tokenName = wrap { currencySymbol, tokenName }

assetToTuple :: AssetClass -> (CurrencySymbol /\ TokenName)
assetToTuple (AssetClass asset) = asset.currencySymbol /\ asset.tokenName

assetToValue :: AssetClass -> BigInt -> Value
assetToValue (AssetClass asset) = Value.singleton asset.currencySymbol asset.tokenName
