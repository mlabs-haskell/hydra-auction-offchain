module HydraAuctionOffchain.Service.Common
  ( AffjaxError(AffjaxError)
  , ArgonautJson(ArgonautJson)
  , ServiceError
      ( ServiceDecodeJsonError
      , ServiceHttpError
      , ServiceHttpResponseError
      )
  ) where

import Prelude

import Affjax (Error, printError) as Affjax
import Affjax.StatusCode (StatusCode) as Affjax
import Data.Argonaut (Json, stringify)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)

data ServiceError
  = ServiceDecodeJsonError String String
  | ServiceHttpError AffjaxError
  | ServiceHttpResponseError Affjax.StatusCode String

derive instance Generic ServiceError _

instance Show ServiceError where
  show = genericShow

newtype AffjaxError = AffjaxError Affjax.Error

derive instance Newtype AffjaxError _

instance Show AffjaxError where
  show = Affjax.printError <<< unwrap

newtype ArgonautJson = ArgonautJson Json

derive instance Newtype ArgonautJson _
derive instance Eq ArgonautJson

instance Show ArgonautJson where
  show = stringify <<< unwrap
