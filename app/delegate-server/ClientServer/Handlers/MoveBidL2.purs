module DelegateServer.ClientServer.Handlers.MoveBidL2
  ( moveBidL2Handler
  ) where

import DelegateServer.State (AppM)
import HTTPure (Response, notFound) as HTTPure

moveBidL2Handler :: AppM HTTPure.Response
moveBidL2Handler = HTTPure.notFound
