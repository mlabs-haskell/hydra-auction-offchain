module HydraAuctionOffchain.Service.Common
  ( AffjaxError(AffjaxError)
  , ArgonautJson(ArgonautJson)
  ) where

import Prelude

import Affjax (Error, printError) as Affjax
import Data.Argonaut (Json, stringify)
import Data.Newtype (class Newtype, unwrap)

newtype AffjaxError = AffjaxError Affjax.Error

derive instance Newtype AffjaxError _

instance Show AffjaxError where
  show = Affjax.printError <<< unwrap

newtype ArgonautJson = ArgonautJson Json

derive instance Newtype ArgonautJson _

instance Show ArgonautJson where
  show = stringify <<< unwrap
