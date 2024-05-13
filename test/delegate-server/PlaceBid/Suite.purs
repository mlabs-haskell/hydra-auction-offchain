module Test.DelegateServer.PlaceBid.Suite
  ( suite
  ) where

import Prelude

import Contract.Config (NetworkId(TestnetId))
import Contract.Monad (Contract, ContractEnv, liftedE)
import Contract.Test (ContractTest, noWallet)
import Contract.Test.Mote (TestPlanM)
import Contract.Time (POSIXTime)
import Contract.Transaction (Language(PlutusV2), Transaction)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask, local)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Plutip.Utils (tmpdir)
import Ctl.Internal.Types.Int (fromInt)
import Data.Codec.Argonaut (JsonCodec, array, encode, int, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (Either(Left, Right), either)
import Data.Generic.Rep (class Generic)
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (modify, unwrap, wrap)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String (contains) as String
import Data.Time.Duration (Minutes(Minutes))
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
import DelegateServer.Lib.Contract (runContractNullCostsAff)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.Types.HydraUtxoMap (HydraUtxoMap, toUtxoMapWithoutRefScripts)
import DelegateServer.WebSocket (WebSocket, mkWebSocket)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.AVar (tryPut) as AVarSync
import Effect.Aff (Aff, launchAff, try)
import Effect.Aff.AVar (empty, take, tryPut) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw, throwException)
import Foreign.Object (insert) as Obj
import HydraAuctionOffchain.Codec (bigIntCodecNum, sysStartCodec)
import HydraAuctionOffchain.Config (HostPort)
import HydraAuctionOffchain.Contract.QueryUtxo (findStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types (AuctionInfo, BidTerms, StandingBidState)
import HydraAuctionOffchain.Helpers (dateTimeFromPosixTimeUnsafe, mkPosixTimeUnsafe, nowPosix)
import HydraAuctionOffchain.Lib.Json (caDecodeFile, readJsonFromFile, writeJsonToFile)
import JS.BigInt (BigInt)
import Mote (group, test)
import Node.ChildProcess (ChildProcess, defaultSpawnOptions, kill, spawn, stdout)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (exists, mkdir, rm') as FSSync
import Node.Path (FilePath)
import Node.Stream (onDataString)
import Test.DelegateServer.PlaceBid.Fixtures
  ( auctionInfoFixture
  , bidFixture0
  , bidFixture1
  , commitUtxoMapFixture0
  , commitUtxoMapFixture1
  )
import Test.Spec.Assertions (fail, shouldEqual)

suite :: TestPlanM ContractTest Unit
suite = do
  group "place-bid" do
    test "valid transition from empty standing bid" do
      runTest commitUtxoMapFixture0 bidFixture0
        mustSucceed
    test "valid transition from non-empty standing bid" do
      runTest commitUtxoMapFixture1 bidFixture1
        mustSucceed

----------------------------------------------------------------------
-- Test

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

runTest
  :: (AuctionInfo -> HydraUtxoMap)
  -> BidTerms
  -> (BidTerms -> TestResult -> Aff Unit)
  -> ContractTest
runTest mkCommitUtxoMap bidTerms callback =
  noWallet $ withWallet appConfig.walletSk $ patchContractEnv do
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
      nowTime
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

----------------------------------------------------------------------
-- Config

type AppConfigTest =
  { hydraNode :: HostPort
  , hydraNodeApi :: HostPort
  , hydraSk :: FilePath
  , cardanoSk :: FilePath
  , walletSk :: FilePath
  , pparams :: FilePath
  , genesisTemplate :: FilePath
  }

appConfig :: AppConfigTest
appConfig =
  { hydraNode: { host: "127.0.0.1", port: UInt.fromInt 7050 }
  , hydraNodeApi: { host: "127.0.0.1", port: UInt.fromInt 7051 }
  , hydraSk: "test/data/keys/hydra.sk"
  , cardanoSk: "test/data/keys/cardano.sk"
  , walletSk: "test/data/keys/wallet.sk"
  , pparams: "protocol-parameters.json"
  , genesisTemplate: "test/data/genesis-template.json"
  }

----------------------------------------------------------------------
-- Hydra node

runHydraNodeOfflineMode
  :: HydraUtxoMap
  -> AVar Unit
  -> POSIXTime
  -> Effect (ChildProcess /\ FilePath)
runHydraNodeOfflineMode initUtxoMap apiServerStartedSem sysStart = do
  nodeId <- UUID.toString <$> UUID.genUUID
  tmpDir <- tmpdir
  let
    workdir = tmpDir <</>> nodeId
    initUtxoFp = workdir <</>> "utxo.json"
  workdirExists <- FSSync.exists workdir
  unless workdirExists (FSSync.mkdir workdir)
  writeJsonToFile initUtxoFp initUtxoMap
  let persistDir = workdir <</>> "persist-dir"
  genesisFp <- genGenesisFromTemplate workdir sysStart
  hydraNodeProcess <- spawn "hydra-node" (hydraNodeArgs nodeId persistDir initUtxoFp genesisFp)
    defaultSpawnOptions
  onDataString (stdout hydraNodeProcess) Encoding.UTF8 \str ->
    when (String.contains (Pattern "APIServerStarted") str) do
      void $ AVarSync.tryPut unit apiServerStartedSem
  pure $ hydraNodeProcess /\ workdir
  where
  hydraNodeArgs :: String -> FilePath -> FilePath -> FilePath -> Array String
  hydraNodeArgs nodeId persistDir initUtxoFp genesisFp =
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
    , appConfig.pparams
    , "--ledger-genesis"
    , genesisFp
    , "--initial-utxo"
    , initUtxoFp
    ]

genGenesisFromTemplate :: FilePath -> POSIXTime -> Effect FilePath
genGenesisFromTemplate workdir sysStartPosix' = do
  genesisTemplate <- either throw pure =<< readJsonFromFile appConfig.genesisTemplate
  let
    sysStartPosix = sysStartPosix' - mkPosixTimeUnsafe (Minutes one)
    sysStart = wrap $ dateTimeFromPosixTimeUnsafe sysStartPosix
    genesisFp = workdir <</>> "genesis.json"
  writeJsonToFile genesisFp $
    Obj.insert "systemStart" (CA.encode sysStartCodec sysStart)
      genesisTemplate
  pure genesisFp

----------------------------------------------------------------------
-- WebSocket

type TestParams =
  { auctionInfo :: AuctionInfo
  , bidTerms :: BidTerms
  , contractEnv :: ContractEnv
  , callback :: TestResult -> Aff Unit
  }

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
  In_SnapshotConfirmed { snapshot } -> do
    let utxos = toUtxoMapWithoutRefScripts (unwrap snapshot).utxo
    liftAff $ params.callback $ SnapshotConfirmed $ snd <$> findStandingBidUtxo
      (unwrap params.auctionInfo)
      utxos
  In_TxInvalid ->
    liftAff $ params.callback TxInvalid
  _ -> pure unit

----------------------------------------------------------------------
-- ContractEnv

patchContractEnv :: forall (a :: Type). Contract a -> Contract a
patchContractEnv contract = do
  pparams <- pparamsSlice
  contract # local \env -> env
    { networkId = TestnetId
    , ledgerConstants =
        env.ledgerConstants
          { pparams =
              env.ledgerConstants.pparams # modify _
                { costModels =
                    wrap $ Map.singleton PlutusV2
                      (wrap $ fromInt <$> pparams.costModels."PlutusV2")
                , maxTxExUnits =
                    { mem: pparams.maxTxExecutionUnits.memory
                    , steps: pparams.maxTxExecutionUnits.steps
                    }
                }
          }
    }

pparamsSlice :: Contract PParamsSlice
pparamsSlice =
  liftedE $ liftEffect $
    caDecodeFile pparamsSliceCodec appConfig.pparams

type PParamsSlice =
  { maxTxExecutionUnits :: { memory :: BigInt, steps :: BigInt }
  , costModels :: { "PlutusV2" :: Array Int }
  }

pparamsSliceCodec :: CA.JsonCodec PParamsSlice
pparamsSliceCodec =
  CA.object "PParamsSlice" $ CAR.record
    { maxTxExecutionUnits:
        CA.object "PParamsSlice:ExUnits" $ CAR.record
          { memory: bigIntCodecNum
          , steps: bigIntCodecNum
          }
    , costModels:
        CA.object "PParamsSlice:CostModels" $ CAR.record
          { "PlutusV2": CA.array CA.int
          }
    }
