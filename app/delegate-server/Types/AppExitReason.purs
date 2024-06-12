module DelegateServer.Types.AppExitReason
  ( AppExitReason
      ( AppExitReason_MissingOrInvalidAuctionInfo
      , AppExitReason_HeadFinalized
      , AppExitReason_BiddingTimeExpired_UnexpectedHeadStatus
      , AppExitReason_Cleanup
      )
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import DelegateServer.Contract.QueryAuction (QueryAuctionError)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus)

data AppExitReason
  = AppExitReason_MissingOrInvalidAuctionInfo QueryAuctionError
  | AppExitReason_HeadFinalized
  | AppExitReason_BiddingTimeExpired_UnexpectedHeadStatus HydraHeadStatus
  | AppExitReason_Cleanup

derive instance Generic AppExitReason _
derive instance Eq AppExitReason

instance Show AppExitReason where
  show = genericShow
