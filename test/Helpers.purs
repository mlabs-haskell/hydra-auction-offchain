module Test.Helpers
  ( chunksOf2
  , defDistribution
  , localhost
  , mkAddressUnsafe
  , mkCurrencySymbolUnsafe
  , mkOrefUnsafe
  , mkPubKeyHashUnsafe
  , mkTokenNameUnsafe
  , mkTokenNameAsciiUnsafe
  , mkVerificationKeyUnsafe
  , mkdirIfNotExists
  , publicPaymentKeyToFile
  , unsafeHead
  , untilM
  , waitUntil
  ) where

import Prelude

import Aeson (encodeAeson)
import Contract.Address (Address, PubKeyHash, Bech32String)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArray, rawBytesToHex)
import Contract.Test (InitialUTxOs)
import Contract.Time (POSIXTime)
import Contract.Transaction (PublicKey, TransactionInput)
import Contract.Value (CurrencySymbol, TokenName, mkCurrencySymbol, mkTokenName)
import Control.Error.Util (bool)
import Control.Monad.Rec.Class (class MonadRec, untilJust)
import Ctl.Internal.Cardano.Types.Transaction (convertPubKey)
import Ctl.Internal.Plutus.Conversion (toPlutusAddress)
import Ctl.Internal.Serialization.Address (addressFromBech32)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashFromBytes)
import Ctl.Internal.Serialization.Keys (bytesFromPublicKey)
import Data.Array (cons, drop, head, take)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import HydraAuctionOffchain.Contract.Types (VerificationKey, vkeyFromBytes)
import HydraAuctionOffchain.Helpers (fromJustWithErr, waitSeconds)
import JS.BigInt (fromInt, toNumber) as BigInt
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (exists, mkdir, writeTextFile) as FSSync
import Node.Path (FilePath)

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

unsafeHead :: forall a. Array a -> a
unsafeHead = fromJustWithErr "unsafeHead" <<< head

-- Delays the fiber until the given POSIXTime has passed (plus 3 seconds).
waitUntil :: POSIXTime -> Contract Unit
waitUntil futureTime = do
  nowTime <- currentTime
  when (nowTime < futureTime) do
    liftAff $ delay $ wrap $
      (BigInt.toNumber $ unwrap $ futureTime - nowTime) + 3000.0

untilM :: forall m a. MonadRec m => MonadAff m => (a -> Boolean) -> m a -> m Unit
untilM p action =
  void $ untilJust do
    res <- action <#> bool Nothing (Just unit) <<< p
    waitSeconds one
    pure res

chunksOf2 :: forall a. Array a -> Maybe (Array (a /\ a))
chunksOf2 xs =
  case take 2 xs of
    [ x, y ] -> cons (x /\ y) <$> chunksOf2 (drop 2 xs)
    [] -> Just []
    _ -> Nothing

publicPaymentKeyToFile :: forall m. MonadEffect m => FilePath -> PublicKey -> m Unit
publicPaymentKeyToFile fp =
  liftEffect
    <<< FSSync.writeTextFile Encoding.UTF8 fp
    <<< formatPublicPaymentKey

formatPublicPaymentKey :: PublicKey -> String
formatPublicPaymentKey key =
  show $ encodeAeson
    { "type": "PaymentVerificationKeyShelley_ed25519"
    , description: "Payment Verification Key"
    , cborHex: keyToCbor key
    }

keyToCbor :: PublicKey -> String
keyToCbor =
  (magicPrefix <> _) <<< rawBytesToHex <<< bytesFromPublicKey
    <<< convertPubKey

magicPrefix :: String
magicPrefix = "5820"

mkdirIfNotExists :: forall m. MonadEffect m => FilePath -> m Unit
mkdirIfNotExists dir =
  liftEffect do
    dirExists <- FSSync.exists dir
    unless dirExists $ FSSync.mkdir dir

localhost :: String
localhost = "127.0.0.1"
