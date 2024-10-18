module DelegateServer.Types.AppExitReason
  ( AppExitReason
      ( AppExitReason_HeadFinalized
      , AppExitReason_BiddingTimeExpired_UnexpectedHeadStatus
      , AppExitReason_Cleanup
      )
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import HydraSdk.Types (HydraHeadStatus)

data AppExitReason
  = AppExitReason_HeadFinalized
  | AppExitReason_BiddingTimeExpired_UnexpectedHeadStatus HydraHeadStatus
  | AppExitReason_Cleanup

derive instance Generic AppExitReason _
derive instance Eq AppExitReason

instance Show AppExitReason where
  show = genericShow
