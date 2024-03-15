module Test.DelegateServer.PlaceBid.Spec where

import Prelude
import Test.DelegateServer.PlaceBid.Fixtures

import Contract.Config (NetworkId(TestnetId))
import Contract.Monad (Contract, ContractEnv)
import Contract.Test (ContractTest, noWallet)
import Contract.Test.Mote (TestPlanM)
import Contract.Transaction (Transaction)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask, local)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils (tmpdir)
import Data.Either (Either(Left, Right), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt, toString) as UInt
import Data.UUID (genUUID, toString) as UUID
import DelegateServer.Contract.PlaceBid (PlaceBidL2ContractError, placeBidL2')
import DelegateServer.HydraNodeApi.Types.Message
  ( HydraNodeApi_InMessage(In_HeadIsOpen, In_SnapshotConfirmed, In_TxInvalid)
  , HydraNodeApi_OutMessage(Out_NewTx)
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  )
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.State (runContractNullCostsAff)
import DelegateServer.Types.HydraUtxoMap (HydraUtxoMap, toUtxoMapWithoutRefScripts)
import DelegateServer.WebSocket (WebSocket, mkWebSocket)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (tryPut) as AVarSync
import Effect.Aff (Aff, launchAff, try)
import Effect.Aff.AVar (empty, take, tryPut) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throwException)
import HydraAuctionOffchain.Config (HostPort)
import HydraAuctionOffchain.Contract.QueryUtxo (findStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types (AuctionInfo, BidTerms, StandingBidState)
import HydraAuctionOffchain.Helpers (nowPosix)
import HydraAuctionOffchain.Lib.Json (writeJsonToFile)
import Mote (group, test)
import Node.ChildProcess (ChildProcess, defaultSpawnOptions, kill, spawn, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (exists, mkdir, rm') as FSSync
import Node.Path (FilePath)
import Node.Stream (onDataString)
import Test.Spec.Assertions (fail, shouldEqual)

suite :: TestPlanM ContractTest Unit
suite = do
  group "place-bid" do
    test "valid transition from empty standing bid" do
      runTest commitUtxoMapFixture0 bidFixture0
        mustSucceed

-- test "valid transition from non-empty standing bid" do
--  runTest commitUtxoMapFixture1 bidFixture1
--    mustSucceed

type AppConfigTest =
  { hydraNode :: HostPort
  , hydraNodeApi :: HostPort
  , hydraSk :: FilePath
  , cardanoSk :: FilePath
  , walletSk :: FilePath
  }

appConfig :: AppConfigTest
appConfig =
  { hydraNode: { host: "127.0.0.1", port: UInt.fromInt 7050 }
  , hydraNodeApi: { host: "127.0.0.1", port: UInt.fromInt 7051 }
  , hydraSk: "test/data/keys/hydra.sk"
  , cardanoSk: "test/data/keys/cardano.sk"
  , walletSk: "test/data/keys/wallet.sk"
  }

data TestResult
  = ContractError PlaceBidL2ContractError
  | TxInvalid
  | SnapshotConfirmed (Maybe StandingBidState)

derive instance Generic TestResult _
derive instance Eq TestResult

instance Show TestResult where
  show = genericShow

mustSucceed :: BidTerms -> TestResult -> Aff Unit
mustSucceed bidTerms = case _ of
  ContractError err ->
    fail $ "PlaceBidL2 contract error: " <> show err <> "."
  TxInvalid ->
    fail $ "PlaceBidL2 tx invalid."
  SnapshotConfirmed Nothing ->
    fail $ "Could not find target standing bid in confirmed snapshot."
  SnapshotConfirmed (Just standingBid) ->
    shouldEqual standingBid $ wrap $ Just bidTerms

type TestParams =
  { auctionInfo :: AuctionInfo
  , bidTerms :: BidTerms
  , contractEnv :: ContractEnv
  , callback :: TestResult -> Aff Unit
  }

runTest
  :: (AuctionInfo -> HydraUtxoMap)
  -> BidTerms
  -> (BidTerms -> TestResult -> Aff Unit)
  -> ContractTest
runTest mkCommitUtxoMap bidTerms callback =
  noWallet $ withWallet appConfig.walletSk $ local _ { networkId = TestnetId } do
    nowTime <- nowPosix
    auctionInfo <- auctionInfoFixture nowTime
    let initUtxoMap = mkCommitUtxoMap auctionInfo
    (testResultReceivedSem :: AVar (Either Error Unit)) <- liftAff AVar.empty
    contractEnv <- ask
    let
      testParams =
        { auctionInfo
        , bidTerms
        , contractEnv
        , callback: \res -> do
            res' <- try $ callback bidTerms res
            void $ AVar.tryPut res' testResultReceivedSem
        }
    (apiServerStartedSem :: AVar Unit) <- liftAff AVar.empty
    hydraNodeProcess /\ workdir <- liftEffect $ runHydraNodeOfflineMode initUtxoMap
      apiServerStartedSem
    liftAff $ AVar.take apiServerStartedSem
    ws <- liftEffect $ mkHydraNodeApiWebSocket testParams
    let
      cleanupHandler = do
        ws.baseWs.close
        kill SIGINT hydraNodeProcess
        FSSync.rm' workdir
          { force: true
          , maxRetries: zero
          , recursive: true
          , retryDelay: zero
          }
    res <- liftAff $ AVar.take testResultReceivedSem
    liftEffect $ cleanupHandler
    either (liftEffect <<< throwException) (const (pure unit)) res

runHydraNodeOfflineMode :: HydraUtxoMap -> AVar Unit -> Effect (ChildProcess /\ FilePath)
runHydraNodeOfflineMode initUtxoMap apiServerStartedSem = do
  nodeId <- UUID.toString <$> UUID.genUUID
  tmpDir <- tmpdir
  let
    workdir = tmpDir <</>> nodeId
    initUtxoFp = workdir <</>> "utxo.json"
  workdirExists <- FSSync.exists workdir
  unless workdirExists (FSSync.mkdir workdir)
  writeJsonToFile initUtxoFp initUtxoMap
  let persistDir = workdir <</>> "persist-dir"
  hydraNodeProcess <- spawn "hydra-node" (hydraNodeArgs nodeId persistDir initUtxoFp)
    defaultSpawnOptions
  onDataString (stdout hydraNodeProcess) Encoding.UTF8 \str -> do
    when (String.contains (Pattern "APIServerStarted") str) do
      void $ AVarSync.tryPut unit apiServerStartedSem
  pure $ hydraNodeProcess /\ workdir
  where
  hydraNodeArgs :: String -> FilePath -> FilePath -> Array String
  hydraNodeArgs nodeId persistDir initUtxoFp =
    [ "offline"
    , "--node-id"
    , nodeId
    , "--host"
    , appConfig.hydraNode.host
    , "--port"
    , UInt.toString appConfig.hydraNode.port
    , "--api-host"
    , appConfig.hydraNodeApi.host
    , "--api-port"
    , UInt.toString appConfig.hydraNodeApi.port
    , "--persistence-dir"
    , persistDir
    , "--hydra-signing-key"
    , appConfig.hydraSk
    , "--cardano-signing-key"
    , appConfig.cardanoSk
    , "--ledger-protocol-parameters"
    , "protocol-parameters.json"
    , "--initial-utxo"
    , initUtxoFp
    ]

type HydraNodeApiWebSocketMock =
  { baseWs :: WebSocket Contract HydraNodeApi_InMessage HydraNodeApi_OutMessage
  , newTx :: Transaction -> Effect Unit
  }

mkHydraNodeApiWebSocket :: TestParams -> Effect HydraNodeApiWebSocketMock
mkHydraNodeApiWebSocket params = do
  ws <- fst <$> mkWebSocket
    { hostPort: appConfig.hydraNodeApi
    , inMsgCodec: hydraNodeApiInMessageCodec
    , outMsgCodec: hydraNodeApiOutMessageCodec
    , runM: void <<< launchAff <<< runContractNullCostsAff params.contractEnv
    }
  let
    hydraNodeApiWs :: HydraNodeApiWebSocketMock
    hydraNodeApiWs =
      { baseWs: ws
      , newTx: ws.send <<< Out_NewTx <<< { transaction: _ }
      }
  ws.onMessage (messageHandler params hydraNodeApiWs)
  pure hydraNodeApiWs

messageHandler
  :: TestParams
  -> HydraNodeApiWebSocketMock
  -> HydraNodeApi_InMessage
  -> Contract Unit
messageHandler params ws = case _ of
  In_HeadIsOpen { utxo } -> do
    let utxos = toUtxoMapWithoutRefScripts utxo
    res <- runExceptT $ placeBidL2' (unwrap params.auctionInfo) params.bidTerms ws.newTx
      utxos
      appConfig.cardanoSk
    case res of
      Left contractError ->
        liftAff $ params.callback $ ContractError contractError
      Right _ ->
        pure unit
  In_SnapshotConfirmed { snapshot: { utxo } } -> do
    let utxos = toUtxoMapWithoutRefScripts utxo
    liftAff $ params.callback $ SnapshotConfirmed $ snd <$> findStandingBidUtxo
      (unwrap params.auctionInfo)
      utxos
  In_TxInvalid ->
    liftAff $ params.callback TxInvalid
  _ -> pure unit
