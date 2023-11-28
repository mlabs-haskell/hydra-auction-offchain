module PlutipEnv.Main
  ( main
  ) where

import Prelude

import Contract.Test.Plutip (InitialUTxOs, withPlutipContractEnv)
import Contract.Wallet (KeyWallet)
import Contract.Wallet.Key (keyWalletPrivatePaymentKey)
import Contract.Wallet.KeyFile (privatePaymentKeyToFile)
import Data.BigInt (fromInt) as BigInt
import Effect (Effect)
import Effect.AVar (tryPut) as AVar
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (empty, take) as AVar
import Effect.Class (liftEffect)
import Effect.Console (log)
import HydraAuctionOffchain.Config (plutipConfig)
import Node.Encoding (Encoding(UTF8))
import Node.Process (stdin)
import Node.Stream (destroy, onDataString)
import Options.Applicative ((<**>))
import Options.Applicative as Optparse
import PlutipEnv.Config (PlutipEnvConfig, configParser)
import PlutipEnv.Server (server)

main :: Effect Unit
main = launchAff_ do
  config <- liftEffect $ Optparse.execParser opts
  withPlutipEnv \wallet -> do
    storeWalletKey config wallet
    closeServer <- liftEffect $ server config
    sem <- AVar.empty
    liftEffect $ onDataString stdin UTF8 \str ->
      when (str == "stop\n") do
        success <- AVar.tryPut unit sem
        when success do
          closeServer $ log "Stopping plutip-env server, finalizing Plutip environment."
    void $ AVar.take sem
    liftEffect $ destroy stdin
  where
  opts :: Optparse.ParserInfo PlutipEnvConfig
  opts =
    Optparse.info (configParser <**> Optparse.helper) $ Optparse.fullDesc
      <> Optparse.header "plutip-env"

withPlutipEnv :: (KeyWallet -> Aff Unit) -> Aff Unit
withPlutipEnv cont =
  withPlutipContractEnv plutipConfig distr \_contractEnv wallet -> cont wallet
  where
  distr :: InitialUTxOs
  distr = [ BigInt.fromInt 2_000_000_000 ]

storeWalletKey :: PlutipEnvConfig -> KeyWallet -> Aff Unit
storeWalletKey config wallet =
  privatePaymentKeyToFile config.paymentSkeyFilePath $ keyWalletPrivatePaymentKey wallet
