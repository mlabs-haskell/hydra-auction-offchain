module Test.Helpers
  ( defDistribution
  , mkAddressUnsafe
  , mkCurrencySymbolUnsafe
  , mkOrefUnsafe
  , mkPubKeyHashUnsafe
  , mkTokenNameUnsafe
  , mkTokenNameAsciiUnsafe
  , mkVerificationKeyUnsafe
  , waitUntil
  ) where

import Prelude

import Contract.Address (Address, PubKeyHash, Bech32String)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArray)
import Contract.Test (InitialUTxOs)
import Contract.Time (POSIXTime)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol, TokenName, mkCurrencySymbol, mkTokenName)
import Ctl.Internal.Plutus.Conversion (toPlutusAddress)
import Ctl.Internal.Serialization.Address (addressFromBech32)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes)
import Data.Newtype (unwrap, wrap)
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import HydraAuctionOffchain.Contract.Types (VerificationKey, vkeyFromBytes)
import HydraAuctionOffchain.Helpers (fromJustWithErr)
import JS.BigInt (fromInt, toNumber) as BigInt

defDistribution :: InitialUTxOs
defDistribution =
  [ BigInt.fromInt 2_000_000_000
  , BigInt.fromInt 2_000_000_000
  ]

mkAddressUnsafe :: Bech32String -> Address
mkAddressUnsafe addr = fromJustWithErr "mkAddressUnsafe" $
  toPlutusAddress =<< addressFromBech32 addr

mkVerificationKeyUnsafe :: String -> VerificationKey
mkVerificationKeyUnsafe vk = fromJustWithErr "mkVerificationKeyUnsafe" $
  vkeyFromBytes =<< hexToByteArray vk

mkPubKeyHashUnsafe :: String -> PubKeyHash
mkPubKeyHashUnsafe pkh =
  fromJustWithErr "mkPubKeyHashUnsafe" $
    (map wrap <<< ed25519KeyHashFromBytes) =<< hexToByteArray pkh

mkCurrencySymbolUnsafe :: String -> CurrencySymbol
mkCurrencySymbolUnsafe cs = fromJustWithErr "mkCurrencySymbolUnsafe" $
  mkCurrencySymbol =<< hexToByteArray cs

mkTokenNameUnsafe :: String -> TokenName
mkTokenNameUnsafe tn = fromJustWithErr "mkTokenNameUnsafe" $
  mkTokenName =<< hexToByteArray tn

mkTokenNameAsciiUnsafe :: String -> TokenName
mkTokenNameAsciiUnsafe tn = fromJustWithErr "mkTokenNameAsciiUnsafe" $
  mkTokenName =<< byteArrayFromAscii tn

mkOrefUnsafe :: String -> TransactionInput
mkOrefUnsafe txHash =
  fromJustWithErr "mkOrefUnsafe" $
    (wrap <<< { transactionId: _, index: zero } <<< wrap) <$> hexToByteArray txHash

-- Delays the fiber until the given POSIXTime has passed (plus 3 seconds).
waitUntil :: POSIXTime -> Contract Unit
waitUntil futureTime = do
  nowTime <- currentTime
  when (nowTime < futureTime) do
    liftAff $ delay $ wrap $
      (BigInt.toNumber $ unwrap $ futureTime - nowTime) + 3000.0
