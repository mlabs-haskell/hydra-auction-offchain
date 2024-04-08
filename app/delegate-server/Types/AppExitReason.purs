module DelegateServer.Types.AppExitReason
  ( AppExitReason
      ( AppExitReason_HeadFinalized
      , AppExitReason_BiddingTimeExpired_UnexpectedHeadStatus
      )
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus)

data AppExitReason
  = AppExitReason_HeadFinalized
  | AppExitReason_BiddingTimeExpired_UnexpectedHeadStatus HydraHeadStatus

derive instance Generic AppExitReason _
derive instance Eq AppExitReason

instance Show AppExitReason where
  show = genericShow
