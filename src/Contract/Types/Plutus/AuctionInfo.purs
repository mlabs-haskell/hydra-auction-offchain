module HydraAuctionOffchain.Contract.Types.Plutus.AuctionInfo
  ( AuctionInfo(AuctionInfo)
  , AuctionInfoExtended(AuctionInfoExtended)
  , AuctionInfoExtendedRec
  , AuctionInfoRec
  , AuctionInfoValidationError
      ( AuctionEscrowAddressMismatchError
      , BidderDepositAddressMismatchError
      , FeeEscrowAddressMismatchError
      , StandingBidAddressMismatchError
      )
  , auctionInfoCodec
  , auctionInfoExtendedCodec
  , mkAuctionInfoExtended
  , validateAuctionInfo
  ) where

import HydraAuctionOffchain.Contract.Types.Plutus.Extra.TypeLevel
import Prelude

import Contract.Address (Address, scriptHashAddress)
import Contract.Numeric.BigNum (zero) as BigNum
import Contract.PlutusData (class FromData, class ToData, PlutusData(Constr))
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (fold, length)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Data.Validation.Semigroup (V)
import HydraAuctionOffchain.Codec (class HasJson, addressCodec, currencySymbolCodec, orefCodec)
import HydraAuctionOffchain.Config (config)
import HydraAuctionOffchain.Contract.Types.Plutus.AuctionTerms
  ( AuctionTerms
  , auctionTermsCodec
  )
import HydraAuctionOffchain.Contract.Types.Plutus.DelegateInfo
  ( DelegateInfo
  , delegateInfoCodec
  )
import HydraAuctionOffchain.Contract.Validators (AuctionValidators)
import HydraAuctionOffchain.Helpers (errV)
import Record (merge) as Record
import Type.Proxy (Proxy(Proxy))

----------------------------------------------------------------------
-- AuctionInfo

type AuctionInfoRec (r :: Row Type) =
  ( auctionId :: CurrencySymbol
  , auctionTerms :: AuctionTerms
  , delegateInfo :: Maybe DelegateInfo
  , auctionEscrowAddr :: Address
  , bidderDepositAddr :: Address
  , feeEscrowAddr :: Address
  , standingBidAddr :: Address
  | r
  )

newtype AuctionInfo = AuctionInfo (Record (AuctionInfoRec ()))

derive instance Generic AuctionInfo _
derive instance Newtype AuctionInfo _
derive instance Eq AuctionInfo

instance Show AuctionInfo where
  show = genericShow

type AuctionInfoSchema =
  ("auctionId" :~: CurrencySymbol)
    :$: ("auctionTerms" :~: AuctionTerms)
    :$: ("delegateInfo" :~: Maybe DelegateInfo)
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

instance HasJson AuctionInfo where
  jsonCodec = const auctionInfoCodec

auctionInfoCodec :: CA.JsonCodec AuctionInfo
auctionInfoCodec =
  wrapIso AuctionInfo $ CA.object "AuctionInfo" $ CAR.record
    { auctionId: currencySymbolCodec
    , auctionTerms: auctionTermsCodec
    , delegateInfo: CA.maybe delegateInfoCodec
    , auctionEscrowAddr: addressCodec config.network
    , bidderDepositAddr: addressCodec config.network
    , feeEscrowAddr: addressCodec config.network
    , standingBidAddr: addressCodec config.network
    }

----------------------------------------------------------------------
-- AuctionInfoExtended

type AuctionInfoExtendedRec = AuctionInfoRec
  ( metadataOref :: Maybe TransactionInput
  )

newtype AuctionInfoExtended = AuctionInfoExtended (Record AuctionInfoExtendedRec)

derive instance Generic AuctionInfoExtended _
derive instance Newtype AuctionInfoExtended _
derive instance Eq AuctionInfoExtended

instance Show AuctionInfoExtended where
  show = genericShow

type AuctionInfoExtendedSchema =
  ("auctionId" :~: CurrencySymbol)
    :$: ("auctionTerms" :~: AuctionTerms)
    :$: ("delegateInfo" :~: Maybe DelegateInfo)
    :$: ("auctionEscrowAddr" :~: Address)
    :$: ("bidderDepositAddr" :~: Address)
    :$: ("feeEscrowAddr" :~: Address)
    :$: ("standingBidAddr" :~: Address)
    :$: ("metadataOref" :~: Maybe TransactionInput)
    :$: Nil

auctionInfoExtendedSchema :: Proxy AuctionInfoExtendedSchema
auctionInfoExtendedSchema = Proxy

instance ToData AuctionInfoExtended where
  toData (AuctionInfoExtended rec) =
    Constr BigNum.zero $ toDataRec auctionInfoExtendedSchema rec

instance FromData AuctionInfoExtended where
  fromData (Constr n pd)
    | n == BigNum.zero && recLength (Proxy :: Proxy AuctionInfoExtended) == length pd =
        wrap <$> fromDataRec auctionInfoExtendedSchema pd
  fromData _ = Nothing

instance HasJson AuctionInfoExtended where
  jsonCodec = const auctionInfoExtendedCodec

auctionInfoExtendedCodec :: CA.JsonCodec AuctionInfoExtended
auctionInfoExtendedCodec =
  wrapIso AuctionInfoExtended $ CA.object "AuctionInfoExtended" $ CAR.record
    { auctionId: currencySymbolCodec
    , auctionTerms: auctionTermsCodec
    , delegateInfo: CA.maybe delegateInfoCodec
    , auctionEscrowAddr: addressCodec config.network
    , bidderDepositAddr: addressCodec config.network
    , feeEscrowAddr: addressCodec config.network
    , standingBidAddr: addressCodec config.network
    , metadataOref: CA.maybe orefCodec
    }

mkAuctionInfoExtended :: AuctionInfo -> Maybe TransactionInput -> AuctionInfoExtended
mkAuctionInfoExtended auctionInfo metadataOref =
  wrap $ Record.merge (unwrap auctionInfo)
    { metadataOref
    }

----------------------------------------------------------------------
-- Validation

data AuctionInfoValidationError
  = AuctionEscrowAddressMismatchError
  | BidderDepositAddressMismatchError
  | FeeEscrowAddressMismatchError
  | StandingBidAddressMismatchError

derive instance Generic AuctionInfoValidationError _
derive instance Eq AuctionInfoValidationError

instance Show AuctionInfoValidationError where
  show = genericShow

validateAuctionInfo
  :: forall (r :: Row Type)
   . Record (AuctionInfoRec r)
  -> AuctionValidators Validator
  -> V (Array AuctionInfoValidationError) Unit
validateAuctionInfo auctionInfo validators = fold
  [ (auctionInfo.auctionEscrowAddr == validatorAddresses.auctionEscrow)
      `errV` AuctionEscrowAddressMismatchError
  , (auctionInfo.bidderDepositAddr == validatorAddresses.bidderDeposit)
      `errV` BidderDepositAddressMismatchError
  , (auctionInfo.feeEscrowAddr == validatorAddresses.feeEscrow)
      `errV` FeeEscrowAddressMismatchError
  , (auctionInfo.standingBidAddr == validatorAddresses.standingBid)
      `errV` StandingBidAddressMismatchError
  ]
  where
  validatorAddresses =
    unwrap $ validators <#> flip scriptHashAddress Nothing <<< validatorHash
