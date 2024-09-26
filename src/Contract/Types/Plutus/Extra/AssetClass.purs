module HydraAuctionOffchain.Contract.Types.Plutus.Extra.AssetClass
  ( AssetClass(AssetClass)
  , assetToTuple
  , assetToValue
  , mkAssetClass
  ) where

import Prelude

import Cardano.Types (AssetName, BigNum, ScriptHash)
import Contract.PlutusData (class FromData, class ToData, PlutusData(List), fromData, toData)
import Contract.Value (Value)
import Contract.Value (singleton) as Value
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))

newtype AssetClass = AssetClass
  { currencySymbol :: ScriptHash
  , tokenName :: AssetName
  }

derive instance Generic AssetClass _
derive instance Newtype AssetClass _
derive instance Eq AssetClass
derive instance Ord AssetClass

instance Show AssetClass where
  show = genericShow

instance ToData AssetClass where
  toData (AssetClass rec) =
    List [ toData rec.currencySymbol, toData rec.tokenName ]

instance FromData AssetClass where
  fromData (List [ cs, tn ]) =
    wrap <$> ({ currencySymbol: _, tokenName: _ } <$> fromData cs <*> fromData tn)
  fromData _ = Nothing

mkAssetClass :: ScriptHash -> AssetName -> AssetClass
mkAssetClass currencySymbol tokenName = wrap { currencySymbol, tokenName }

assetToValue :: AssetClass -> BigNum -> Value
assetToValue (AssetClass asset) = Value.singleton asset.currencySymbol asset.tokenName

assetToTuple :: AssetClass -> (ScriptHash /\ AssetName)
assetToTuple (AssetClass asset) = asset.currencySymbol /\ asset.tokenName
