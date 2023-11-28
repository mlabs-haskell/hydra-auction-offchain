module HydraAuctionOffchain.WalletApp
  ( WalletApp(Nami, Gero, Flint, Eternl, Lode, NuFi, Lace, Plutip)
  , walletAppCodec
  , walletSpecFromWalletApp
  ) where

import Prelude

import Contract.Config
  ( PrivatePaymentKeySource(PrivatePaymentKeyValue)
  , WalletSpec
      ( ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      , ConnectToNuFi
      , ConnectToLace
      , UseKeys
      )
  , ServerConfig
  )
import Data.Codec.Argonaut (JsonCodec, prismaticCodec, string) as CA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.String.Read (class Read, read)
import Effect.Aff (Aff)
import HydraAuctionOffchain.Codec (class HasJson)
import HydraAuctionOffchain.Service.PlutipEnv (queryPlutipEnvPrivateKey)

data WalletApp = Nami | Gero | Flint | Eternl | Lode | NuFi | Lace | Plutip

derive instance Generic WalletApp _
derive instance Eq WalletApp

instance Show WalletApp where
  show = genericShow

instance Read WalletApp where
  read = case _ of
    "Nami" -> Just Nami
    "Gero" -> Just Gero
    "Flint" -> Just Flint
    "Eternl" -> Just Eternl
    "Lode" -> Just Lode
    "NuFi" -> Just NuFi
    "Lace" -> Just Lace
    "Plutip" -> Just Plutip
    _ -> Nothing

instance HasJson WalletApp where
  jsonCodec = const walletAppCodec

walletAppCodec :: CA.JsonCodec WalletApp
walletAppCodec = CA.prismaticCodec "WalletApp" read show CA.string

walletSpecFromWalletApp :: ServerConfig -> WalletApp -> Aff WalletSpec
walletSpecFromWalletApp plutipEnvServerConfig = case _ of
  Nami -> pure ConnectToNami
  Gero -> pure ConnectToGero
  Flint -> pure ConnectToFlint
  Eternl -> pure ConnectToEternl
  Lode -> pure ConnectToLode
  NuFi -> pure ConnectToNuFi
  Lace -> pure ConnectToLace
  Plutip ->
    queryPlutipEnvPrivateKey plutipEnvServerConfig
      <#> flip UseKeys Nothing <<< PrivatePaymentKeyValue
