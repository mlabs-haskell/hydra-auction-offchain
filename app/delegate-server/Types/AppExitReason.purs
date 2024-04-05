module DelegateServer.Types.AppExitReason
  ( AppExitReason
      ( AppExitReason_HeadFinalized
      )
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data AppExitReason = AppExitReason_HeadFinalized

derive instance Generic AppExitReason _
derive instance Eq AppExitReason

instance Show AppExitReason where
  show = genericShow
