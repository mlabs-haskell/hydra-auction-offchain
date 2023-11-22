module HydraAuctionOffchain.Contract.Types.Plutus.AuctionInfo
  ( AuctionInfo(AuctionInfo)
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (Address)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Value (CurrencySymbol)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms (AuctionTerms)
import Type.Proxy (Proxy(Proxy))

newtype AuctionInfo = AuctionInfo
  { auctionId :: CurrencySymbol
  , auctionTerms :: AuctionTerms
  , auctionEscrowAddr :: Address
  , bidderDepositAddr :: Address
  , feeEscrowAddr :: Address
  , standingBidAddr :: Address
  }

derive instance Generic AuctionInfo _
derive instance Newtype AuctionInfo _
derive instance Eq AuctionInfo

instance Show AuctionInfo where
  show = genericShow

type AuctionInfoSchema =
  ("auctionId" :~: CurrencySymbol)
    :$: ("auctionTerms" :~: AuctionTerms)
    :$: ("auctionEscrowAddr" :~: Address)
    :$: ("bidderDepositAddr" :~: Address)
    :$: ("feeEscrowAddr" :~: Address)
    :$: ("standingBidAddr" :~: Address)
    :$: Nil

auctionInfoSchema :: Proxy AuctionInfoSchema
auctionInfoSchema = Proxy

instance ToData AuctionInfo where
  toData (AuctionInfo rec) = Constr BigNum.zero $ toDataRec auctionInfoSchema rec

instance FromData AuctionInfo where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: Proxy AuctionInfo) == length pd =
        wrap <$> fromDataRec auctionInfoSchema pd
  fromData _ = Nothing
