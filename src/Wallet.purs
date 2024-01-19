module HydraAuctionOffchain.Wallet
  ( SignMessageError
      ( CouldNotGetWalletAddressError
      , SignDataFailedError
      , CouldNotGetSigFromCoseSign1Error
      , CouldNotDecodeCoseKeyError
      , CouldNotGetPubKeyFromCoseKeyError
      , CouldNotGetWalletPubKeyHashError
      , VkPkhMismatchError
      , CouldNotBuildSigStructError
      , CouldNotVerifySignatureError
      , InvalidSignatureError
      )
  , SignMessageResult
  , askWalletVk
  , askWalletVk'
  , signMessage
  ) where

import Prelude

import Contract.Address (Address, PubKeyHash, toPubKeyHash)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray, CborBytes, byteArrayFromAscii)
import Contract.Wallet (getWalletAddress, signData)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, throwError)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Ctl.Internal.Plutus.Types.Address (getAddress)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Config (config)
import HydraAuctionOffchain.Contract.Types (VerificationKey, vkeyBytes, vkeyFromBytes)
import HydraAuctionOffchain.Helpers ((!*))
import HydraAuctionOffchain.Lib.Cose (getCoseSign1Signature, mkSigStructure)
import HydraAuctionOffchain.Lib.Crypto (hashVk, verifySignature)
import Partial.Unsafe (unsafePartial)

foreign import data CoseKey :: Type
foreign import fromBytesCoseKey :: CborBytes -> Effect CoseKey
foreign import getCoseKeyHeaderX :: MaybeFfiHelper -> CoseKey -> Maybe ByteArray

type SignMessageResult =
  { signature :: ByteArray -- actual signature, not COSESign1 structure
  , vkey :: VerificationKey
  , address :: Address
  , pkh :: PubKeyHash
  }

data SignMessageError
  = CouldNotGetWalletAddressError
  | SignDataFailedError
  | CouldNotGetSigFromCoseSign1Error
  | CouldNotDecodeCoseKeyError
  | CouldNotGetPubKeyFromCoseKeyError
  | CouldNotGetWalletPubKeyHashError
  | VkPkhMismatchError
  | CouldNotBuildSigStructError
  | CouldNotVerifySignatureError
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
  -- Get wallet address:
  addrPlutus <- getWalletAddress !? CouldNotGetWalletAddressError
  let addr = fromPlutusAddress config.network addrPlutus

  -- Sign data, extract vkey and signature:
  { key, signature: coseSign1 } <- signData addr (wrap payload) !* SignDataFailedError
  signature <- liftEffect (getCoseSign1Signature $ unwrap coseSign1)
    !* CouldNotGetSigFromCoseSign1Error
  coseKey <- liftEffect (fromBytesCoseKey key) !* CouldNotDecodeCoseKeyError
  vkey <- (vkeyFromBytes =<< getCoseKeyHeaderX maybeFfiHelper coseKey)
    ?? CouldNotGetPubKeyFromCoseKeyError
  let vkeyBytes' = vkeyBytes vkey

  -- Check `pkh == hash vkey`:
  pkh <- toPubKeyHash (getAddress addrPlutus) ?? CouldNotGetWalletPubKeyHashError
  when (Just pkh /= hashVk vkeyBytes') $ throwError VkPkhMismatchError

  -- Verify signature:
  sigStruct <- liftEffect (mkSigStructure addrPlutus payload) !* CouldNotBuildSigStructError
  success <- liftEffect (verifySignature vkeyBytes' sigStruct signature)
    !* CouldNotVerifySignatureError
  unless success $ throwError InvalidSignatureError

  pure
    { signature
    , vkey
    , pkh
    , address: addrPlutus
    }
