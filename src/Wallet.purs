module HydraAuctionOffchain.Wallet
  ( GetWalletPubKeyBytesError
      ( CouldNotGetWalletAddressError
      , CouldNotGetWalletPubKeyHashError
      , NonAsciiMessageError
      , SignDataFailedError
      , CouldNotDecodeCoseKeyError
      , CouldNotGetPubKeyFromCoseKeyError
      )
  , getWalletPubKeyBytes
  ) where

import Prelude

import Contract.Address (PubKeyHash, toPubKeyHash)
import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray, CborBytes, rawBytesFromAscii)
import Contract.Wallet (getWalletAddressWithNetworkTag, signData)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddressWithNetworkTag)
import Ctl.Internal.Plutus.Types.Address (getAddress)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Helpers ((!*))

foreign import data CoseKey :: Type
foreign import fromBytesCoseKey :: CborBytes -> Effect CoseKey
foreign import getCoseKeyHeaderX :: MaybeFfiHelper -> CoseKey -> Maybe ByteArray

data GetWalletPubKeyBytesError
  = CouldNotGetWalletAddressError
  | CouldNotGetWalletPubKeyHashError
  | NonAsciiMessageError
  | SignDataFailedError
  | CouldNotDecodeCoseKeyError
  | CouldNotGetPubKeyFromCoseKeyError

derive instance Generic GetWalletPubKeyBytesError _
derive instance Eq GetWalletPubKeyBytesError

instance Show GetWalletPubKeyBytesError where
  show = genericShow

getWalletPubKeyBytes
  :: String
  -> ExceptT GetWalletPubKeyBytesError Contract (PubKeyHash /\ ByteArray)
getWalletPubKeyBytes message = do
  addrPlutus <- getWalletAddressWithNetworkTag !? CouldNotGetWalletAddressError
  pkh <- toPubKeyHash (getAddress addrPlutus) ?? CouldNotGetWalletPubKeyHashError
  payload <- rawBytesFromAscii message ?? NonAsciiMessageError
  let addr = fromPlutusAddressWithNetworkTag addrPlutus
  { key: coseKeyBytes } <- signData addr payload !? SignDataFailedError
  coseKey <- liftEffect (fromBytesCoseKey coseKeyBytes) !* CouldNotDecodeCoseKeyError
  vkey <- getCoseKeyHeaderX maybeFfiHelper coseKey ?? CouldNotGetPubKeyFromCoseKeyError
  pure $ pkh /\ vkey
