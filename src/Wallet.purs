module HydraAuctionOffchain.Wallet
  ( GetWalletPubKeyBytesError
      ( CouldNotGetWalletAddressError
      , SignDataFailedError
      , CouldNotDecodeCoseKeyError
      , CouldNotGetPubKeyFromCoseKeyError
      )
  , getWalletPubKeyBytes
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Prim.ByteArray (ByteArray, CborBytes)
import Contract.Wallet (getWalletAddressWithNetworkTag, signData)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT)
import Ctl.Internal.FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddressWithNetworkTag)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Helpers ((!*))

foreign import data CoseKey :: Type
foreign import fromBytesCoseKey :: CborBytes -> Effect CoseKey
foreign import getCoseKeyHeaderX :: MaybeFfiHelper -> CoseKey -> Maybe ByteArray

data GetWalletPubKeyBytesError
  = CouldNotGetWalletAddressError
  | SignDataFailedError
  | CouldNotDecodeCoseKeyError
  | CouldNotGetPubKeyFromCoseKeyError

derive instance Generic GetWalletPubKeyBytesError _
derive instance Eq GetWalletPubKeyBytesError

instance Show GetWalletPubKeyBytesError where
  show = genericShow

getWalletPubKeyBytes :: ExceptT GetWalletPubKeyBytesError Contract ByteArray
getWalletPubKeyBytes = do
  addrPlutus <- getWalletAddressWithNetworkTag !? CouldNotGetWalletAddressError
  let addr = fromPlutusAddressWithNetworkTag addrPlutus
  { key: coseKeyBytes } <- signData addr mempty !? SignDataFailedError
  coseKey <- liftEffect (fromBytesCoseKey coseKeyBytes) !* CouldNotDecodeCoseKeyError
  getCoseKeyHeaderX maybeFfiHelper coseKey ?? CouldNotGetPubKeyFromCoseKeyError
