module HydraAuctionOffchain.Types.WalletApp
  ( WalletApp(Nami, Gero, Flint, Eternl, Lode, NuFi, Lace)
  , walletAppCodec
  , walletSpecFromWalletApp
  ) where

import Prelude

import Contract.Config
  ( WalletSpec
      ( ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToEternl
      , ConnectToLode
      , ConnectToNuFi
      , ConnectToLace
      )
  )
import Data.Codec.Argonaut (JsonCodec, prismaticCodec, string) as CA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.String.Read (class Read, read)
import HydraAuctionOffchain.Lib.Codec (class HasJson)

data WalletApp = Nami | Gero | Flint | Eternl | Lode | NuFi | Lace

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
    _ -> Nothing

instance HasJson WalletApp anyParams where
  jsonCodec _ = const walletAppCodec

walletAppCodec :: CA.JsonCodec WalletApp
walletAppCodec = CA.prismaticCodec "WalletApp" read show CA.string

walletSpecFromWalletApp :: WalletApp -> WalletSpec
walletSpecFromWalletApp = case _ of
  Nami -> ConnectToNami
  Gero -> ConnectToGero
  Flint -> ConnectToFlint
  Eternl -> ConnectToEternl
  Lode -> ConnectToLode
  NuFi -> ConnectToNuFi
  Lace -> ConnectToLace
