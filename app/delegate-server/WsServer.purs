module DelegateServer.WsServer
  ( DelegateWebSocketServer
  , DelegateWebSocketServerMessage
      ( HydraHeadStatus
      , StandingBid
      )
  , WsServerAppMap
  , auctionCsDecodingFailure
  , auctionCsMismatch
  , delegateWsServerMessageCodec
  , wsServer
  , wsServerGeneric
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types (NetworkId, ScriptHash)
import Contract.CborBytes (hexToCborBytes)
import Data.Array (singleton, (:))
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right))
import Data.Generic.Rep (class Generic)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String (stripPrefix) as String
import Data.Tuple (snd)
import Data.Variant (inj, match) as Variant
import DelegateServer.App (runApp)
import DelegateServer.AppManager.Types (AppManager', ActiveAuction)
import DelegateServer.Config (Network, networkToNetworkId)
import DelegateServer.Contract.StandingBid (queryStandingBidL2)
import DelegateServer.Lib.WebSocketServer
  ( WebSocketCloseReason
  , WebSocketOnConnectAction(AcceptWebSocketConn, CloseWebSocketConn)
  , WebSocketServer
  , WebSocketServerOptions
  , mkWebSocketServer
  )
import DelegateServer.State (readAppState)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus, headStatusCodec)
import Effect (Effect)
import Effect.AVar (tryRead) as AVarSync
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Contract.Types (StandingBidState, standingBidStateCodec)
import Type.Proxy (Proxy(Proxy))
import URI.Port (Port)
import URI.Port (toInt) as Port

auctionCsDecodingFailure :: WebSocketCloseReason
auctionCsDecodingFailure =
  { code: 4000
  , reason: "Could not decode Auction-Cs from the query string"
  }

auctionCsMismatch :: WebSocketCloseReason
auctionCsMismatch =
  { code: 4001
  , reason:
      "Provided Auction-Cs does not correspond to any auction served \
      \by this delegate server"
  }

type WsServerAppMap appState =
  { lookupApp :: ScriptHash -> Effect (Maybe appState)
  , getHeadStatus :: appState -> Aff HydraHeadStatus
  , getStandingBid :: appState -> Aff (Maybe StandingBidState)
  }

wsServer
  :: forall a
   . Port
  -> Network
  -> AVar (AppManager' a DelegateWebSocketServer)
  -> Aff DelegateWebSocketServer
wsServer wsServerPort network appManagerAvar =
  wsServerGeneric wsServerPort network appMap
  where
  appMap :: WsServerAppMap (ActiveAuction a)
  appMap =
    { lookupApp: \auctionCs -> do
        activeAuctions <- map (_.activeAuctions <<< unwrap) <$> AVarSync.tryRead appManagerAvar
        pure $ Map.lookup auctionCs =<< activeAuctions
    , getHeadStatus: \{ appState, appLogger } ->
        runApp appState appLogger $ readAppState (Proxy :: _ "headStatus")
    , getStandingBid: \{ appState, appLogger } ->
        runApp appState appLogger $ map snd <$> queryStandingBidL2
    }

wsServerGeneric
  :: forall appState
   . Port
  -> Network
  -> WsServerAppMap appState
  -> Aff DelegateWebSocketServer
wsServerGeneric wsServerPort network appMap = do
  wss <- mkWebSocketServer (delegateWsServerMessageCodec $ networkToNetworkId network)
    wsServerOptions
  liftEffect do
    wss.onConnect \{ connPath, sendMessages } -> do
      let auctionCsRaw = fromMaybe connPath $ String.stripPrefix (Pattern "/") connPath
      case decodeCbor =<< hexToCborBytes auctionCsRaw of
        Nothing -> do
          pure $ CloseWebSocketConn auctionCsDecodingFailure
        Just auctionCs ->
          appMap.lookupApp auctionCs >>= case _ of
            Nothing -> do
              pure $ CloseWebSocketConn auctionCsMismatch
            Just appState ->
              AcceptWebSocketConn <$ launchAff_ do
                headStatus <- appMap.getHeadStatus appState
                standingBid <- appMap.getStandingBid appState
                liftEffect do
                  sendMessages $
                    HydraHeadStatus headStatus
                      : maybe mempty (singleton <<< StandingBid) standingBid
  pure wss
  where
  wsServerOptions :: WebSocketServerOptions
  wsServerOptions =
    { host: "0.0.0.0"
    , port: Port.toInt wsServerPort
    }

type DelegateWebSocketServer = WebSocketServer DelegateWebSocketServerMessage

data DelegateWebSocketServerMessage
  = HydraHeadStatus HydraHeadStatus
  | StandingBid StandingBidState

derive instance Generic DelegateWebSocketServerMessage _
derive instance Eq DelegateWebSocketServerMessage

instance Show DelegateWebSocketServerMessage where
  show = genericShow

delegateWsServerMessageCodec :: NetworkId -> CA.JsonCodec DelegateWebSocketServerMessage
delegateWsServerMessageCodec network =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "HydraHeadStatus": Right headStatusCodec
        , "StandingBid": Right $ standingBidStateCodec network
        }
    )
  where
  toVariant = case _ of
    HydraHeadStatus x ->
      Variant.inj (Proxy :: _ "HydraHeadStatus") x
    StandingBid x ->
      Variant.inj (Proxy :: _ "StandingBid") x

  fromVariant = Variant.match
    { "HydraHeadStatus": HydraHeadStatus
    , "StandingBid": StandingBid
    }
