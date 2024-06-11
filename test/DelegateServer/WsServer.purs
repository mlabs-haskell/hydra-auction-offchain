module Test.DelegateServer.WsServer
  ( suite
  ) where

import Prelude

import Contract.Config (NetworkId(MainnetId))
import Contract.Value (CurrencySymbol)
import Control.Monad.Logger.Trans (LoggerT, runLoggerT)
import Data.Array (cons, elemIndex, singleton) as Array
import Data.Codec.Argonaut (null) as CA
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (fst)
import DelegateServer.Config (Network(Mainnet))
import DelegateServer.Lib.AVar (modifyAVar_)
import DelegateServer.Lib.WebSocketServer (WebSocketCloseReason)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus)
import DelegateServer.WebSocket (WebSocketBuilder, mkWebSocket)
import DelegateServer.WsServer
  ( DelegateWebSocketServer
  , DelegateWebSocketServerMessage(HydraHeadStatus, StandingBid)
  , WsServerAppMap
  , auctionCsDecodingFailure
  , auctionCsMismatch
  , delegateWsServerMessageCodec
  , wsServerGeneric
  )
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (empty, new, tryPut, tryRead) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import HydraAuctionOffchain.Contract.Types (StandingBidState)
import HydraAuctionOffchain.Helpers (csToHex, waitSeconds)
import Mote (MoteT, group, test)
import Test.Gen (genStandingBid)
import Test.Helpers (mkCurrencySymbolUnsafe)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSampleOne)
import Test.Spec.Assertions (shouldReturn)
import URI.Port (Port)
import URI.Port (print, unsafeFromInt) as Port

suite :: MoteT Aff (Aff Unit) Aff Unit
suite =
  group "ws-server" do
    test "sends hydra head status on connection" do
      headStatus <- liftEffect $ randomSampleOne arbitrary
      withWsServer [ auctionCsFixture0 ] headStatus Nothing \_ ->
        connWsServerTrackMessages (csToHex auctionCsFixture0) \messages -> do
          waitSeconds one
          AVar.tryRead messages `shouldReturn`
            Just (Array.singleton $ HydraHeadStatus headStatus)

    test "sends hydra head status and standing bid on connection" do
      headStatus <- liftEffect $ randomSampleOne arbitrary
      standingBid <- liftEffect $ randomSampleOne genStandingBid
      withWsServer [ auctionCsFixture0 ] headStatus (Just standingBid) \_ ->
        connWsServerTrackMessages (csToHex auctionCsFixture0) \messages -> do
          waitSeconds one
          AVar.tryRead messages `shouldReturn`
            Just [ StandingBid standingBid, HydraHeadStatus headStatus ]

    test "closes connection on auction currency symbol decoding failure" do
      headStatus <- liftEffect $ randomSampleOne arbitrary
      withWsServer mempty headStatus Nothing \_ ->
        connWsServerExpectClose "invalidCs" \closeReason -> do
          waitSeconds one
          AVar.tryRead closeReason `shouldReturn`
            Just auctionCsDecodingFailure

    test "closes connection on auction currency mismatch" do
      headStatus <- liftEffect $ randomSampleOne arbitrary
      withWsServer [ auctionCsFixture0 ] headStatus Nothing \_ ->
        connWsServerExpectClose (csToHex auctionCsFixture1) \closeReason -> do
          waitSeconds one
          AVar.tryRead closeReason `shouldReturn`
            Just auctionCsMismatch

wsServerPort :: Port
wsServerPort = Port.unsafeFromInt 7080

withWsServer
  :: Array CurrencySymbol
  -> HydraHeadStatus
  -> Maybe StandingBidState
  -> (DelegateWebSocketServer -> Aff Unit)
  -> Aff Unit
withWsServer auctionsToServe headStatus standingBid cont = do
  let appMap = appMapMock auctionsToServe headStatus standingBid
  wsServer <- wsServerGeneric wsServerPort Mainnet appMap
  cont wsServer
  wsServer.close

appMapMock
  :: Array CurrencySymbol
  -> HydraHeadStatus
  -> Maybe StandingBidState
  -> WsServerAppMap Unit
appMapMock auctionsToServe headStatus standingBid = do
  { lookupApp: \auctionCs -> unit <$ Array.elemIndex auctionCs auctionsToServe
  , getHeadStatus: const (pure headStatus)
  , getStandingBid: const (pure standingBid)
  }

connWsServerTrackMessages
  :: String
  -> (AVar (Array DelegateWebSocketServerMessage) -> Aff Unit)
  -> Aff Unit
connWsServerTrackMessages auctionCs cont = do
  messages <- AVar.new mempty
  ws <- liftEffect $ fst <$> mkWebSocket (wsBuilder auctionCs)
  liftEffect $ ws.onMessage $ \m -> modifyAVar_ messages (pure <<< Array.cons m)
  cont messages
  liftEffect ws.close

connWsServerExpectClose :: String -> (AVar WebSocketCloseReason -> Aff Unit) -> Aff Unit
connWsServerExpectClose auctionCs cont = do
  closeReason <- AVar.empty
  ws <- liftEffect $ fst <$> mkWebSocket (wsBuilder auctionCs)
  liftEffect $ ws.onClose \code reason ->
    liftAff $ void $ AVar.tryPut { code, reason } closeReason
  cont closeReason

wsBuilder :: String -> WebSocketBuilder (LoggerT Aff) DelegateWebSocketServerMessage Unit
wsBuilder auctionCs =
  { url: "ws://127.0.0.1" <> Port.print wsServerPort <> "/" <> auctionCs
  , inMsgCodec: delegateWsServerMessageCodec MainnetId
  , outMsgCodec: CA.null
  , runM: launchAff_ <<< void <<< flip runLoggerT (const (pure unit))
  }

auctionCsFixture0 :: CurrencySymbol
auctionCsFixture0 =
  mkCurrencySymbolUnsafe
    "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65"

auctionCsFixture1 :: CurrencySymbol
auctionCsFixture1 =
  mkCurrencySymbolUnsafe
    "92c4f22371bd453aec9fe19ccebfbc88211ae854b5eab424bcd4c26d"
