module DelegateServer.Handlers.MoveBid
  ( moveBidHandler
  ) where

import Prelude

import DelegateServer.Contract.Commit (commitStandingBid)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (AppM, becomeCommitLeader, readAppState)
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus(HeadStatus_Idle, HeadStatus_Initializing)
  )
import Effect.Class (liftEffect)
import HTTPure (Response, created) as HTTPure

moveBidHandler :: HydraNodeApiWebSocket -> AppM HTTPure.Response
moveBidHandler ws = do
  becomeCommitLeader
  readAppState _.headStatus >>= case _ of
    HeadStatus_Idle -> liftEffect ws.initHead
    HeadStatus_Initializing -> commitStandingBid
    _ -> pure unit
  HTTPure.created
