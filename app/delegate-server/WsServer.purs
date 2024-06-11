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

import Contract.Config (NetworkId)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Value (CurrencySymbol, mkCurrencySymbol)
import Data.Array (singleton, (:))
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Right))
import Data.Generic.Rep (class Generic)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String (stripPrefix) as String
import Data.Tuple (fst, snd)
import Data.Variant (inj, match) as Variant
import DelegateServer.App (AppLogger, runApp)
import DelegateServer.AppMap (AppMap)
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
import Effect.Aff (Aff, launchAff_)
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
  { lookupApp :: CurrencySymbol -> Maybe appState
  , getHeadStatus :: appState -> Aff HydraHeadStatus
  , getStandingBid :: appState -> Aff (Maybe StandingBidState)
  }

wsServer :: Port -> Network -> AppMap Unit -> AppLogger -> Aff DelegateWebSocketServer
wsServer wsServerPort network appMap appLogger =
  wsServerGeneric wsServerPort network
    { lookupApp: \auctionCs ->
        fst <$> Map.lookup auctionCs appMap
    , getHeadStatus: \appState ->
        runApp appState appLogger $ readAppState (Proxy :: _ "headStatus")
    , getStandingBid: \appState ->
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
      case mkCurrencySymbol =<< hexToByteArray auctionCsRaw of
        Nothing -> do
          pure $ CloseWebSocketConn auctionCsDecodingFailure
        Just auctionCs ->
          case appMap.lookupApp auctionCs of
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
