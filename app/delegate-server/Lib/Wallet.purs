module DelegateServer.Lib.Wallet
  ( withWallet
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Wallet (PrivatePaymentKeySource(PrivatePaymentKeyFile), WalletSpec(UseKeys))
import Control.Monad.Reader (local)
import Ctl.Internal.Wallet.Spec (mkWalletBySpec)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff.Class (liftAff)
import Node.Path (FilePath)

withWallet :: forall (a :: Type). FilePath -> Contract a -> Contract a
withWallet sk contract = do
  wallet <- liftAff $ mkWalletBySpec $ UseKeys (PrivatePaymentKeyFile sk) Nothing
  local _ { wallet = Just $ wallet } contract
