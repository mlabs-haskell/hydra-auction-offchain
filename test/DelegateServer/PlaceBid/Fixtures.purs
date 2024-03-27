module Test.DelegateServer.PlaceBid.Fixtures
  ( auctionInfoFixture
  , bidFixture0
  , bidFixture1
  , commitUtxoMapFixture0
  , commitUtxoMapFixture1
  ) where

import Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (OutputDatum(NoOutputDatum, OutputDatum), toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTime)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.Value (CurrencySymbol)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Control.Monad.Except (runExceptT)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Days(Days))
import Data.Tuple.Nested (type (/\), (/\))
import DelegateServer.Const (appConst) as DelegateServer
import DelegateServer.Types.HydraUtxoMap (HydraUtxoMap)
import HydraAuctionOffchain.Contract.MintingPolicies (standingBidTokenName)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfo(AuctionInfo)
  , AuctionTerms
  , BidTerms
  , StandingBidState(StandingBidState)
  )
import HydraAuctionOffchain.Contract.Validators (mkAuctionValidators)
import HydraAuctionOffchain.Helpers (mkPosixTimeUnsafe)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt) as BigInt
import Test.Helpers
  ( mkAddressUnsafe
  , mkCurrencySymbolUnsafe
  , mkOrefUnsafe
  , mkPubKeyHashUnsafe
  , mkTokenNameUnsafe
  , mkVerificationKeyUnsafe
  )

bidFixture1 :: BidTerms
bidFixture1 = wrap
  { bidder: wrap
      { bidderAddress:
          mkAddressUnsafe
            "addr_test1qpsjljwwefv553ly43alu97cd5wtjvfdvqk9f76la7gejgw5p2hj0e0fx5lp95vk3055zcm\
            \y8edf63uwpe37zhcnjvrsev08p0"
      , bidderVk:
          mkVerificationKeyUnsafe
            "c059d4c74359ffe1b8326e03065ab08b0b8c2579cc32e29cf087257b59ec79e9"
      }
  , price: BigInt.fromInt 15_000_000
  , bidderSignature:
      hexToByteArrayUnsafe
        "04d80101455aac2d81890f41e7a3e8c1c76691061428dc4138e01488c592aa88752187556676f29d12aaf\
        \2d8ce27c536612cf43a6394ce5a472c1e9e93c4a50d"
  , sellerSignature:
      hexToByteArrayUnsafe
        "ec1066a6ea9476f7628ac2eeb90fb93a0da09d86e597115b666d18fcc3925801afddcd39e89b768d49cc1\
        \872d44427145d3c0adfc5c45a2f520319c63a93240e"
  }

bidFixture0 :: BidTerms
bidFixture0 = wrap
  { bidder: wrap
      { bidderAddress:
          mkAddressUnsafe
            "addr_test1qpsjljwwefv553ly43alu97cd5wtjvfdvqk9f76la7gejgw5p2hj0e0fx5lp95vk3055zcm\
            \y8edf63uwpe37zhcnjvrsev08p0"
      , bidderVk:
          mkVerificationKeyUnsafe
            "c059d4c74359ffe1b8326e03065ab08b0b8c2579cc32e29cf087257b59ec79e9"
      }
  , price: BigInt.fromInt 10_000_000
  , bidderSignature:
      hexToByteArrayUnsafe
        "f2009f85f65812afdc8a9a51c081533b49378de06e642e60e95694fa3f7ded5b05b000c47ad961385152a\
        \d10a349aeb71eed2e0fbb270b27a6281769c2df1b07"
  , sellerSignature:
      hexToByteArrayUnsafe
        "ec1066a6ea9476f7628ac2eeb90fb93a0da09d86e597115b666d18fcc3925801afddcd39e89b768d49cc1\
        \872d44427145d3c0adfc5c45a2f520319c63a93240e"
  }

commitUtxoMapFixture0 :: AuctionInfo -> HydraUtxoMap
commitUtxoMapFixture0 auctionInfo =
  wrap
    [ bidUtxoFixture0 auctionInfo
    , collateralUtxoFixture DelegateServer.appConst.collateralLovelace
    ]

commitUtxoMapFixture1 :: AuctionInfo -> HydraUtxoMap
commitUtxoMapFixture1 auctionInfo =
  wrap
    [ bidUtxoFixture1 auctionInfo
    , collateralUtxoFixture $ BigInt.fromInt 8_870_780
    ]

collateralUtxoFixture :: BigInt -> TransactionInput /\ TransactionOutput
collateralUtxoFixture lovelace =
  oref /\
    ( wrap
        { address:
            mkAddressUnsafe
              "addr_test1vzcnxhr5u6ej3jzecxsef9pgr4g8nf0lxv92p53qluxdmjqtlwpvx"
        , amount: Value.lovelaceValueOf lovelace
        , datum: NoOutputDatum
        , referenceScript: Nothing
        }
    )
  where
  oref :: TransactionInput
  oref =
    mkOrefUnsafe
      "3c55b6ce414f79ca8aacff50f12a006ca3a2abea746418ab6fcffa28fe372243"

bidUtxoFixture0 :: AuctionInfo -> TransactionInput /\ TransactionOutput
bidUtxoFixture0 (AuctionInfo auctionInfo) =
  oref /\
    ( wrap
        { address: auctionInfo.standingBidAddr
        , amount:
            Value.lovelaceValueOf (BigInt.fromInt 1_099_050)
              <> Value.singleton auctionCsFixture standingBidTokenName one
        , datum: OutputDatum $ wrap $ toData $ StandingBidState Nothing
        , referenceScript: Nothing
        }
    )
  where
  oref :: TransactionInput
  oref =
    mkOrefUnsafe
      "c4ed299e233d356731df46951744f46f3b6eff703a72aa12200a6c4517671d28"

bidUtxoFixture1 :: AuctionInfo -> TransactionInput /\ TransactionOutput
bidUtxoFixture1 (AuctionInfo auctionInfo) =
  oref /\
    ( wrap
        { address: auctionInfo.standingBidAddr
        , amount:
            Value.lovelaceValueOf (BigInt.fromInt 2_228_270)
              <> Value.singleton auctionCsFixture standingBidTokenName one
        , datum:
            OutputDatum $ wrap $ toData $
              StandingBidState (Just bidFixture0)
        , referenceScript: Nothing
        }
    )
  where
  oref :: TransactionInput
  oref =
    mkOrefUnsafe
      "21f1a833c6a1f996332db62092f9c78455a55a85b501d035dd2e7d6250fba0dd"

auctionInfoFixture :: POSIXTime -> Contract AuctionInfo
auctionInfoFixture biddingStart = do
  let auctionTerms = auctionTermsFixture biddingStart
  validators <-
    liftedE $ runExceptT $
      mkAuctionValidators auctionCsFixture auctionTerms
  let
    validatorHashes = validatorHash <$> validators
    validatorAddresses = unwrap $ flip scriptHashAddress Nothing <$> validatorHashes
    auctionInfo = wrap
      { auctionId: auctionCsFixture
      , auctionTerms
      , auctionEscrowAddr: validatorAddresses.auctionEscrow
      , bidderDepositAddr: validatorAddresses.bidderDeposit
      , feeEscrowAddr: validatorAddresses.feeEscrow
      , standingBidAddr: validatorAddresses.standingBid
      }
  pure auctionInfo

auctionCsFixture :: CurrencySymbol
auctionCsFixture =
  mkCurrencySymbolUnsafe
    "2143328cbadfa6089c30c45089a6e4048c4ff05620c0ada52ee8d518"

auctionTermsFixture :: POSIXTime -> AuctionTerms
auctionTermsFixture nowTime = wrap
  { auctionLot:
      Value.singleton
        (mkCurrencySymbolUnsafe "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d")
        (mkTokenNameUnsafe "4d6f6e614c697361")
        one
  , sellerAddress:
      mkAddressUnsafe
        "addr_test1qpsjljwwefv553ly43alu97cd5wtjvfdvqk9f76la7gejgw5p2hj0e0fx5lp95vk3055zcmy8ed\
        \f63uwpe37zhcnjvrsev08p0"
  , sellerVk:
      mkVerificationKeyUnsafe
        "c059d4c74359ffe1b8326e03065ab08b0b8c2579cc32e29cf087257b59ec79e9"
  , delegates:
      [ mkPubKeyHashUnsafe
          "ac55de689702d745e77050ce83b77ff9619383bb802e40fb90aa3be4"
      ]
  , biddingStart: zero
  , biddingEnd: nowTime + mkPosixTimeUnsafe (Days one)
  , purchaseDeadline: nowTime + mkPosixTimeUnsafe (Days 2.0)
  , cleanup: nowTime + mkPosixTimeUnsafe (Days 3.0)
  , auctionFeePerDelegate: BigInt.fromInt 3_000_000
  , startingBid: BigInt.fromInt 8_000_000
  , minBidIncrement: BigInt.fromInt 1_000_000
  , minDepositAmount: BigInt.fromInt 3_000_000
  }
