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
import Cardano.AsCbor (decodeCbor)
import Cardano.Types
  ( Address
  , AssetName
  , Bech32String
  , Ed25519KeyHash
  , PublicKey
  , ScriptHash
  , TransactionInput
  )
import Cardano.Types.Address (fromBech32) as Address
import Cardano.Types.AssetName (mkAssetName)
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.PublicKey (toRawBytes) as PublicKey
import Contract.CborBytes (hexToCborBytes)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArray, rawBytesToHex)
import Contract.Test (InitialUTxOs)
import Contract.Time (POSIXTime)
import Control.Error.Util (bool)
import Control.Monad.Rec.Class (class MonadRec, untilJust)
import Data.Array (cons, drop, head, take)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import HydraAuctionOffchain.Contract.Types (VerificationKey, vkeyFromBytes)
import HydraAuctionOffchain.Helpers (fromJustWithErr, waitSeconds)
import JS.BigInt (toNumber) as BigInt
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (exists, mkdir, writeTextFile) as FSSync
import Node.Path (FilePath)

defDistribution :: InitialUTxOs
defDistribution =
  [ BigNum.fromInt 2_000_000_000
  , BigNum.fromInt 2_000_000_000
  ]

mkAddressUnsafe :: Bech32String -> Address
mkAddressUnsafe addr = fromJustWithErr "mkAddressUnsafe" $
  Address.fromBech32 addr

mkVerificationKeyUnsafe :: String -> VerificationKey
mkVerificationKeyUnsafe vk = fromJustWithErr "mkVerificationKeyUnsafe" $
  vkeyFromBytes =<< hexToByteArray vk

mkPubKeyHashUnsafe :: String -> Ed25519KeyHash
mkPubKeyHashUnsafe pkh = fromJustWithErr "mkPubKeyHashUnsafe" $
  decodeCbor =<< hexToCborBytes pkh

mkCurrencySymbolUnsafe :: String -> ScriptHash
mkCurrencySymbolUnsafe cs = fromJustWithErr "mkCurrencySymbolUnsafe" $
  decodeCbor =<< hexToCborBytes cs

mkTokenNameUnsafe :: String -> AssetName
mkTokenNameUnsafe tn = fromJustWithErr "mkTokenNameUnsafe" $
  mkAssetName =<< hexToByteArray tn

mkTokenNameAsciiUnsafe :: String -> AssetName
mkTokenNameAsciiUnsafe tn = fromJustWithErr "mkTokenNameAsciiUnsafe" $
  mkAssetName =<< byteArrayFromAscii tn

mkOrefUnsafe :: String -> TransactionInput
mkOrefUnsafe txHash =
  fromJustWithErr "mkOrefUnsafe" do
    transactionId <- decodeCbor =<< hexToCborBytes txHash
    pure $ wrap
      { transactionId
      , index: zero
      }

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
keyToCbor = append magicPrefix <<< rawBytesToHex <<< PublicKey.toRawBytes

magicPrefix :: String
magicPrefix = "5820"

mkdirIfNotExists :: forall m. MonadEffect m => FilePath -> m Unit
mkdirIfNotExists dir =
  liftEffect do
    dirExists <- FSSync.exists dir
    unless dirExists $ FSSync.mkdir dir

localhost :: String
localhost = "127.0.0.1"
