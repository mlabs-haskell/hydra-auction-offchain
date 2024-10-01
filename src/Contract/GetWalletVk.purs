module HydraAuctionOffchain.Contract.GetWalletVk
  ( GetWalletVkContractError(GetWalletVk_Error_CouldNotGetWalletVk)
  , getWalletVk
  , getWalletVkWithErrors
  ) where

import Contract.Prelude

import Cardano.Types (PublicKey)
import Contract.Monad (Contract)
import Control.Monad.Except (ExceptT, withExceptT)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , ContractOutput
  , VerificationKey
  , mkContractOutput
  )
import HydraAuctionOffchain.Wallet (SignMessageError, askWalletVk)

getWalletVk :: Contract (ContractOutput PublicKey)
getWalletVk = mkContractOutput identity getWalletVkWithErrors

getWalletVkWithErrors :: ExceptT GetWalletVkContractError Contract PublicKey
getWalletVkWithErrors =
  _.vkey <$>
    withExceptT GetWalletVk_Error_CouldNotGetWalletVk
      askWalletVk

----------------------------------------------------------------------
-- Errors

data GetWalletVkContractError = GetWalletVk_Error_CouldNotGetWalletVk SignMessageError

derive instance Generic GetWalletVkContractError _
derive instance Eq GetWalletVkContractError

instance Show GetWalletVkContractError where
  show = genericShow

instance ToContractError GetWalletVkContractError where
  errorCodePrefix = const "GetWalletVk"
  errorMessage = case _ of
    GetWalletVk_Error_CouldNotGetWalletVk err ->
      "Could not get wallet verification key, error: " <> show err <> "."
