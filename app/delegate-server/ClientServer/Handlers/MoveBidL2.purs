module DelegateServer.ClientServer.Handlers.MoveBidL2
  ( moveBidL2Handler
  ) where

import Prelude

import DelegateServer.Contract.CommitStandingBid (commitStandingBid)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.State (AppM, askHeadStatus, becomeCommitLeader)
import DelegateServer.Types.HydraHeadStatus
  ( HydraHeadStatus(HeadStatus_Idle, HeadStatus_Initializing)
  )
import Effect.Class (liftEffect)
import HTTPure (Response, created) as HTTPure

moveBidL2Handler :: HydraNodeApiWebSocket -> AppM HTTPure.Response
moveBidL2Handler ws = do
  becomeCommitLeader
  askHeadStatus >>= case _ of
    HeadStatus_Idle -> liftEffect ws.initHead
    HeadStatus_Initializing -> commitStandingBid
    _ -> pure unit
  HTTPure.created
