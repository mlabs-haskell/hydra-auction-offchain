module HydraAuctionOffchain.Wallet
  ( SignMessageError
      ( CouldNotGetWalletAddressError
      , CouldNotGetWalletPubKeyHashError
      , SignDataFailedError
      , CouldNotDecodeCoseKeyError
      , CouldNotGetPubKeyFromCoseKeyError
      , VkPkhMismatchError
      )
  , SignMessageResult
  , signMessage
  ) where

import Prelude

import Contract.Address (PubKeyHash, toPubKeyHash)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray, CborBytes)
import Contract.Wallet (getWalletAddressWithNetworkTag, signData)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, throwError)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddressWithNetworkTag)
import Ctl.Internal.Plutus.Types.Address (getAddress)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just))
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Helpers ((!*))
import HydraAuctionOffchain.Lib.Crypto (hashVk)

foreign import data CoseKey :: Type
foreign import fromBytesCoseKey :: CborBytes -> Effect CoseKey
foreign import getCoseKeyHeaderX :: MaybeFfiHelper -> CoseKey -> Maybe ByteArray

type SignMessageResult =
  { signature :: ByteArray
  , pkh :: PubKeyHash
  , vkey :: ByteArray
  }

data SignMessageError
  = CouldNotGetWalletAddressError
  | CouldNotGetWalletPubKeyHashError
  | SignDataFailedError
  | CouldNotDecodeCoseKeyError
  | CouldNotGetPubKeyFromCoseKeyError
  | VkPkhMismatchError

derive instance Generic SignMessageError _
derive instance Eq SignMessageError

instance Show SignMessageError where
  show = genericShow

signMessage :: ByteArray -> ExceptT SignMessageError Contract SignMessageResult
signMessage payload = do
  addrPlutus <- getWalletAddressWithNetworkTag !? CouldNotGetWalletAddressError
  pkh <- toPubKeyHash (getAddress addrPlutus) ?? CouldNotGetWalletPubKeyHashError
  let addr = fromPlutusAddressWithNetworkTag addrPlutus
  { key, signature } <- signData addr (wrap payload) !? SignDataFailedError
  coseKey <- liftEffect (fromBytesCoseKey key) !* CouldNotDecodeCoseKeyError
  vkey <- getCoseKeyHeaderX maybeFfiHelper coseKey ?? CouldNotGetPubKeyFromCoseKeyError
  when (Just pkh /= hashVk vkey) $ throwError VkPkhMismatchError
  pure
    { signature: unwrap signature
    , pkh
    , vkey
    }
