module HydraAuctionOffchain.Types.WalletApp
  ( WalletApp(WalletApp)
  , walletAppCodec
  , walletSpecFromWalletApp
  ) where

import Prelude

import Contract.Config (WalletSpec(ConnectToGenericCip30))
import Contract.Wallet (KnownWallet(Nami, Gero, Flint, Eternl, Lode, Lace, NuFi), walletName)
import Data.Codec.Argonaut (JsonCodec, prismaticCodec, string) as CA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Read (class Read, read)
import HydraAuctionOffchain.Lib.Codec (class HasJson)

newtype WalletApp = WalletApp KnownWallet

derive instance Generic WalletApp _
derive instance Newtype WalletApp _

-- FIXME: add Eq instance for KnownWallet upstream
-- derive instance Eq WalletApp

-- FIXME: add Show instance for KnownWallet upstream
instance Show WalletApp where
  show = unwrap >>> case _ of
    Nami -> "Nami"
    Gero -> "Gero"
    Flint -> "Flint"
    Eternl -> "Eternl"
    Lode -> "Lode"
    NuFi -> "NuFi"
    Lace -> "Lace"

instance Read WalletApp where
  read = map wrap <<< case _ of
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
walletSpecFromWalletApp walletApp =
  ConnectToGenericCip30 (walletName $ unwrap walletApp)
    { cip95: false
    }
