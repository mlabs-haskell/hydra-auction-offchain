module HydraAuctionOffchain.Wallet
  ( SignMessageError
      ( CouldNotGetWalletPkhError
      , CouldNotGetSigFromCoseSign1Error
      , CouldNotDecodeEd25519Signature
      , CouldNotDecodeCoseKeyError
      , CouldNotGetPubKeyFromCoseKeyError
      , VkPkhMismatchError
      , CouldNotBuildSigStructError
      , InvalidSignatureError
      )
  , SignMessageResult
  , askWalletVk
  , askWalletVk'
  , signMessage
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types
  ( Address
  , Credential(PubKeyHashCredential)
  , Ed25519KeyHash
  , Ed25519Signature
  , PublicKey
  , RawBytes
  )
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.PublicKey (fromRawBytes, hash, verify) as PublicKey
import Contract.Address (getNetworkId)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray, CborBytes, byteArrayFromAscii)
import Contract.Wallet (ownPaymentPubKeyHashes, signData)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.State.Trans (lift)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Data.Array (head) as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Helpers ((!*))
import HydraAuctionOffchain.Lib.Cose (getCoseSign1Signature, mkSigStructure)
import Partial.Unsafe (unsafePartial)

foreign import data CoseKey :: Type
foreign import fromBytesCoseKey :: CborBytes -> Effect CoseKey
foreign import getCoseKeyHeaderX :: MaybeFfiHelper -> CoseKey -> Maybe RawBytes

type SignMessageResult =
  { signature :: Ed25519Signature -- actual signature, not COSESign1 structure
  , vkey :: PublicKey
  , address :: Address
  , pkh :: Ed25519KeyHash
  }

data SignMessageError
  = CouldNotGetWalletPkhError
  | CouldNotGetSigFromCoseSign1Error
  | CouldNotDecodeEd25519Signature
  | CouldNotDecodeCoseKeyError
  | CouldNotGetPubKeyFromCoseKeyError
  | VkPkhMismatchError
  | CouldNotBuildSigStructError
  | InvalidSignatureError

derive instance Generic SignMessageError _
derive instance Eq SignMessageError

instance Show SignMessageError where
  show = genericShow

askWalletVk :: ExceptT SignMessageError Contract SignMessageResult
askWalletVk = askWalletVk' mempty

askWalletVk' :: String -> ExceptT SignMessageError Contract SignMessageResult
askWalletVk' extraMessage =
  signMessageAscii $
    consentMessage <> extraMessage
  where
  consentMessage =
    "By signing this message, you authorize hydra-auction to read \
    \your public key."

  signMessageAscii =
    signMessage <<< unsafePartial fromJust <<< byteArrayFromAscii

signMessage :: ByteArray -> ExceptT SignMessageError Contract SignMessageResult
signMessage payload = do
  -- Get wallet payment public key hash:
  pkh <- (map unwrap <<< Array.head <$> ownPaymentPubKeyHashes) !? CouldNotGetWalletPkhError
  network <- lift getNetworkId
  let address = mkPaymentAddress network (wrap $ PubKeyHashCredential pkh) Nothing

  -- Sign data, extract vkey and signature:
  { key, signature: coseSign1 } <- lift $ signData address (wrap payload)
  signatureBytes <- liftEffect (getCoseSign1Signature $ unwrap coseSign1)
    !* CouldNotGetSigFromCoseSign1Error
  signature <- decodeCbor (wrap signatureBytes) ?? CouldNotDecodeEd25519Signature
  coseKey <- liftEffect (fromBytesCoseKey key) !* CouldNotDecodeCoseKeyError
  vkey <- (PublicKey.fromRawBytes =<< getCoseKeyHeaderX maybeFfiHelper coseKey)
    ?? CouldNotGetPubKeyFromCoseKeyError

  -- Check `pkh == hash vkey`:
  when (pkh /= PublicKey.hash vkey) $ throwError VkPkhMismatchError

  -- Verify signature:
  sigStruct <- liftEffect (mkSigStructure address payload)
    !* CouldNotBuildSigStructError
  let success = PublicKey.verify vkey (wrap sigStruct) signature
  unless success $ throwError InvalidSignatureError

  pure
    { signature
    , vkey
    , pkh
    , address
    }
