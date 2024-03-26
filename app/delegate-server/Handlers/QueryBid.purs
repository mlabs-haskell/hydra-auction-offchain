module DelegateServer.Handlers.QueryBid
  ( queryBidHandler
  ) where

import Prelude

import Data.Typelevel.Undefined (undefined)
import DelegateServer.State (class AppOpen)
import HTTPure (Response) as HTTPure

queryBidHandler :: forall m. AppOpen m => m HTTPure.Response
queryBidHandler = undefined
