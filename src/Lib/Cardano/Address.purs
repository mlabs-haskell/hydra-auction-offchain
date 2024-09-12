module HydraAuctionOffchain.Lib.Cardano.Address
  ( scriptHashAddress
  , toPubKeyHash
  ) where

import Prelude

import Cardano.Types
  ( Address
  , Credential(ScriptHashCredential)
  , Ed25519KeyHash
  , NetworkId
  , ScriptHash
  )
import Cardano.Types.Address (getPaymentCredential, mkPaymentAddress)
import Cardano.Types.Credential (asPubKeyHash)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)

toPubKeyHash :: Address -> Maybe Ed25519KeyHash
toPubKeyHash = asPubKeyHash <<< unwrap <=< getPaymentCredential

scriptHashAddress :: NetworkId -> ScriptHash -> Address
scriptHashAddress network scriptHash =
  mkPaymentAddress network
    (wrap $ ScriptHashCredential scriptHash)
    Nothing
