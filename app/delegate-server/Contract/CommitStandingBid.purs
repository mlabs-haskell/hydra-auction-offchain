module DelegateServer.Contract.CommitStandingBid
  ( commitStandingBid
  ) where

import Contract.Prelude

import DelegateServer.State (AppM)

commitStandingBid :: AppM Unit
commitStandingBid = pure unit
