module HydraAuctionOffchain.Config
  ( config
  , mkContractParams
  ) where

import Prelude

import Contract.Config
  ( ContractParams
  , LogLevel(Trace)
  , NetworkId(MainnetId, TestnetId)
  , ServerConfig
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreprodServerConfig
  , defaultConfirmTxDelay
  , defaultTimeParams
  , disabledSynchronizationParams
  , emptyHooks
  , mkBlockfrostBackendParams
  )
import Control.Error.Util (bool)
import Data.Maybe (Maybe(Just, Nothing))
import HydraAuctionOffchain.WalletApp (WalletApp, walletSpecFromWalletApp)

foreign import _isMainnet :: Boolean
foreign import _blockfrostApiKey :: String

type Config =
  { network :: NetworkId
  , blockfrostApiKey :: String
  }

config :: Config
config =
  { network: bool TestnetId MainnetId _isMainnet
  , blockfrostApiKey: _blockfrostApiKey
  }

blockfrostConfig :: ServerConfig
blockfrostConfig =
  case config.network of
    TestnetId ->
      blockfrostPublicPreprodServerConfig
    MainnetId ->
      blockfrostPublicMainnetServerConfig

mkContractParams :: Maybe WalletApp -> ContractParams
mkContractParams walletApp =
  { backendParams: mkBlockfrostBackendParams
      { blockfrostConfig
      , blockfrostApiKey: Just config.blockfrostApiKey
      , confirmTxDelay: defaultConfirmTxDelay
      }
  , networkId: config.network
  , logLevel: Trace
  , walletSpec: walletSpecFromWalletApp <$> walletApp
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , timeParams: defaultTimeParams
  , synchronizationParams: disabledSynchronizationParams
  }
