module DelegateServer.Types.CommitStatus
  ( CommitStatus
      ( ShouldCommitCollateral
      , ShouldCommitStandingBid
      )
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data CommitStatus
  = ShouldCommitCollateral
  | ShouldCommitStandingBid

derive instance Generic CommitStatus _
derive instance Eq CommitStatus

instance Show CommitStatus where
  show = genericShow
