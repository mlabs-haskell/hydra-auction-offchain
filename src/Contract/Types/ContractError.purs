module HydraAuctionOffchain.Contract.Types.ContractError
  ( class ToContractError
  , ContractError
  , toContractError
  ) where

class ToContractError a where
  toContractError :: a -> ContractError

type ContractError =
  { errorCode :: String
  , message :: String
  }
