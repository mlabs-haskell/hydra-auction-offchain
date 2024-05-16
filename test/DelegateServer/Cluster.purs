module Test.DelegateServer.Cluster
  ( TestAppHandle
  , withWallets'
  ) where

import Prelude

import Contract.Address (PubKeyHash)
import Contract.Config (NetworkId(MainnetId), mkCtlBackendParams)
import Contract.Hashing (publicKeyHash)
import Contract.Monad (Contract, ContractEnv, liftContractM, liftedE, runContractInEnv)
import Contract.Test (class UtxoDistribution, ContractTest(ContractTest))
import Contract.Test.Plutip (PlutipConfig)
import Contract.Transaction (Language(PlutusV2), TransactionInput)
import Contract.Wallet (PrivatePaymentKey)
import Contract.Wallet.Key (publicKeyFromPrivateKey)
import Contract.Wallet.KeyFile (privatePaymentKeyToFile)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, local)
import Control.Parallel (parTraverse, parTraverse_)
import Ctl.Internal.Helpers (concatPaths, (<</>>))
import Ctl.Internal.Plutip.Types (ClusterStartupParameters)
import Ctl.Internal.Plutip.Utils (tmpdir)
import Ctl.Internal.Types.Int (fromInt)
import Ctl.Internal.Wallet.Key (KeyWallet(KeyWallet))
import Data.Array (concat, deleteAt, replicate)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, head, toArray) as NEArray
import Data.Codec.Argonaut (JsonCodec, array, int, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (length)
import Data.Int (decimal, toStringAs)
import Data.Log.Level (LogLevel(Info, Warn))
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (modify, unwrap, wrap)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Data.UUID (genUUID, toString) as UUID
import DelegateServer.App (AppM, runApp, runContract)
import DelegateServer.Config (AppConfig, Network(Mainnet))
import DelegateServer.Contract.StandingBid (queryStandingBidL2)
import DelegateServer.Handlers.MoveBid (MoveBidResponse, moveBidHandlerImpl)
import DelegateServer.Handlers.PlaceBid (PlaceBidResponse, placeBidHandlerImpl)
import DelegateServer.Main (AppHandle, startDelegateServer)
import DelegateServer.State (access, readAppState)
import DelegateServer.Types.AppExitReason (AppExitReason)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.AVar (tryRead) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import HydraAuctionOffchain.Codec (bigIntCodecNum)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended
  , BidTerms
  , StandingBidState
  , bidTermsCodec
  )
import HydraAuctionOffchain.Helpers (fromJustWithErr, randomElem)
import HydraAuctionOffchain.Lib.Json (caDecodeFile, caEncodeString)
import JS.BigInt (BigInt)
import Node.Buffer (toString) as Buffer
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8)) as Encoding
import Node.FS.Sync (rm') as FSSync
import Node.Path (FilePath)
import Test.Helpers
  ( chunksOf2
  , defDistribution
  , localhost
  , mkdirIfNotExists
  , publicPaymentKeyToFile
  )
import Test.Plutip.Config (plutipConfig)
import Test.QuickCheck.Gen (chooseInt, randomSampleOne)
import Type.Proxy (Proxy(Proxy))
import URI.Port (toInt, unsafeFromInt) as Port

type TestAppHandle =
  { getActiveAuction :: Contract (Maybe AuctionInfoExtended)
  , getHeadStatus :: Contract HydraHeadStatus
  , moveBidToL2 :: Contract MoveBidResponse
  , queryStandingBidL2 :: Contract (Maybe StandingBidState)
  , placeBidL2 :: BidTerms -> Contract PlaceBidResponse
  , getAppExitReason :: Contract (Maybe AppExitReason)
  }

withWallets'
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> ( wallets
       -> Array PubKeyHash
       -> (AuctionInfoExtended -> (TestAppHandle -> Contract Unit) -> Contract Unit)
       -> Contract Unit
     )
  -> ContractTest
withWallets' distr tests =
  ContractTest \h ->
    let
      numDelegates =
        unsafePerformEffect $ randomSampleOne $ chooseInt 1 3
      distrDelegates =
        concat $ replicate numDelegates [ defDistribution, defDistribution ]
    in
      h (distr /\ distrDelegates) \mPlutipClusterParams (wallets /\ delegateWallets) -> do
        contractEnv <- ask
        plutipClusterParams <-
          liftContractM "Could not get Plutip cluster params"
            mPlutipClusterParams
        delegateWalletsGrouped <-
          liftContractM "Expected even number of delegate wallets"
            (chunksOf2 delegateWallets)
        let
          delegates =
            delegateWalletsGrouped <#> \((KeyWallet kw) /\ _) ->
              publicKeyHash $ publicKeyFromPrivateKey $ unwrap kw.paymentKey
        tests wallets delegates \auctionInfo action -> do
          auctionMetadataOref <-
            liftContractM "Could not get auction metadata oref"
              (unwrap auctionInfo).metadataOref
          let
            clusterConfig =
              { auctionMetadataOref
              , plutipClusterParams
              , plutipConfig
              }
            peers =
              fromJustWithErr "withWallets'" $ NEArray.fromArray $
                delegateWalletsGrouped <#> \(cardanoKw /\ walletKw) ->
                  { cardanoSk: (unwrap cardanoKw).paymentKey
                  , walletSk: (unwrap walletKw).paymentKey
                  }
          liftAff $ withDelegateServerCluster
            contractEnv
            clusterConfig
            peers
            action

withDelegateServerCluster
  :: ContractEnv
  -> DelegateServerClusterConfig
  -> NonEmptyArray DelegateServerPeer
  -> (TestAppHandle -> Contract Unit)
  -> Aff Unit
withDelegateServerCluster contractEnv clusterConfig peers action =
  bracket
    (startDelegateServerCluster clusterConfig peers)
    ( \(appHandles /\ workdirCleanupHandler) ->
        parTraverse_ (liftEffect <<< _.cleanupHandler) appHandles
          *> liftEffect workdirCleanupHandler
    )
    ( \(apps /\ _) ->
        let
          withRandomAppHandle :: forall a. (AppHandle -> AppM a) -> Contract a
          withRandomAppHandle f =
            liftAff do
              appHandle <- randomElem apps
              runApp appHandle.appState appHandle.appLogger $ f appHandle

          withRandomAppHandle_ :: forall a. AppM a -> Contract a
          withRandomAppHandle_ =
            withRandomAppHandle
              <<< const
        in
          runContractInEnv contractEnv $ action
            { getActiveAuction:
                withRandomAppHandle_ do
                  liftAff <<< AVar.tryRead =<< access (Proxy :: _ "auctionInfo")

            , getHeadStatus:
                withRandomAppHandle_ do
                  readAppState (Proxy :: _ "headStatus")

            , moveBidToL2:
                withRandomAppHandle \appHandle ->
                  moveBidHandlerImpl appHandle.ws

            , queryStandingBidL2:
                withRandomAppHandle_ do
                  map snd <$> queryStandingBidL2

            , placeBidL2:
                \bidTerms ->
                  withRandomAppHandle \appHandle -> do
                    env <- wrap <$> runContract (patchContractEnv MainnetId)
                    let body = caEncodeString (bidTermsCodec MainnetId) bidTerms
                    local (_ { contractEnv = env }) $ placeBidHandlerImpl appHandle.ws body

            , getAppExitReason:
                withRandomAppHandle_ do
                  liftAff <<< AVar.tryRead =<< access (Proxy :: _ "exitSem")
            }
    )

type DelegateServerPeer =
  { cardanoSk :: PrivatePaymentKey
  , walletSk :: PrivatePaymentKey
  }

type DelegateServerClusterConfig =
  { auctionMetadataOref :: TransactionInput
  , plutipClusterParams :: ClusterStartupParameters
  , plutipConfig :: PlutipConfig
  }

startDelegateServerCluster
  :: DelegateServerClusterConfig
  -> NonEmptyArray DelegateServerPeer
  -> Aff (Array AppHandle /\ Effect Unit)
startDelegateServerCluster clusterConfig peers
  | length peers > 5 =
      throwError $ error "Unsupported number of peers"
  | otherwise = do
      clusterId <- liftEffect $ UUID.toString <$> UUID.genUUID
      workdir <- liftEffect $ tmpdir <#> flip concatPaths clusterId
      mkdirIfNotExists workdir
      appConfigs <- genDelegateServerConfigs workdir clusterConfig peers
      appHandles <- parTraverse startDelegateServer appConfigs
      let
        workdirCleanupHandler = do
          log $ "workdir: " <> workdir
          FSSync.rm' workdir
            { force: true
            , maxRetries: zero
            , recursive: true
            , retryDelay: zero
            }
      pure $ appHandles /\ workdirCleanupHandler

genDelegateServerConfigs
  :: FilePath
  -> DelegateServerClusterConfig
  -> NonEmptyArray DelegateServerPeer
  -> Aff (Array AppConfig)
genDelegateServerConfigs clusterWorkdir clusterConfig peers = do
  peers' <- createWorkdirsStoreKeys
  hydraScriptsTxHash <- publishHydraScripts
    clusterConfig.plutipClusterParams.nodeSocketPath
    (ops.mkCardanoSk $ snd $ NEArray.head peers')
  pure $ worker (NEArray.toArray peers') hydraScriptsTxHash
  where
  ops =
    { mkServerPort: \idx -> Port.unsafeFromInt (7040 + idx)
    , mkWsServerPort: \idx -> Port.unsafeFromInt (7050 + idx)
    , mkHydraNode: \idx -> { host: localhost, port: UInt.fromInt (7060 + idx) }
    , mkHydraNodeApi: \idx -> { host: localhost, port: UInt.fromInt (7070 + idx) }
    , mkPersistDir: flip concatPaths "persist-dir"
    , mkHydra: flip concatPaths "hydra"
    , mkHydraSk: flip concatPaths "hydra.sk"
    , mkHydraVk: flip concatPaths "hydra.vk"
    , mkCardanoSk: flip concatPaths "cardano.sk"
    , mkCardanoVk: flip concatPaths "cardano.vk"
    , mkWalletSk: flip concatPaths "wallet.sk"
    }

  worker :: Array (Int /\ FilePath) -> String -> Array AppConfig
  worker peers' hydraScriptsTxHash =
    peers' <#> \(idx /\ workdir) -> wrap
      { auctionMetadataOref: clusterConfig.auctionMetadataOref
      , serverPort: ops.mkServerPort idx
      , wsServerPort: ops.mkWsServerPort idx
      , hydraNodeId: toStringAs decimal idx
      , hydraNode: ops.mkHydraNode idx
      , hydraNodeApi: ops.mkHydraNodeApi idx
      , hydraPersistDir: ops.mkPersistDir workdir
      , hydraSk: ops.mkHydraSk workdir
      , cardanoSk: ops.mkCardanoSk workdir
      , walletSk: ops.mkWalletSk workdir
      , peers:
          fromJustWithErr "genDelegateServerConfigs" $
            deleteAt idx peers' <#> map \(idx' /\ workdir') ->
              { hydraNode: ops.mkHydraNode idx'
              , hydraVk: ops.mkHydraVk workdir'
              , cardanoVk: ops.mkCardanoVk workdir'
              , httpServer:
                  { port: UInt.fromInt $ Port.toInt $ ops.mkServerPort idx'
                  , host: localhost
                  , secure: false
                  , path: Nothing
                  }
              }
      , nodeSocket: clusterConfig.plutipClusterParams.nodeSocketPath
      , network: Mainnet
      , queryBackend:
          mkCtlBackendParams
            { ogmiosConfig:
                clusterConfig.plutipConfig.ogmiosConfig
            , kupoConfig:
                clusterConfig.plutipConfig.kupoConfig
            }
      , hydraScriptsTxHash
      , hydraContestPeriod: 5
      , logLevel: Info
      , ctlLogLevel: Warn
      }

  createWorkdirsStoreKeys :: Aff (NonEmptyArray (Int /\ FilePath))
  createWorkdirsStoreKeys =
    peers # traverseWithIndex \idx peer -> do
      let
        nodeId = toStringAs decimal idx
        workdir = clusterWorkdir <</>> nodeId
        cardanoVk = publicKeyFromPrivateKey $ unwrap peer.cardanoSk
      mkdirIfNotExists workdir
      privatePaymentKeyToFile (ops.mkCardanoSk workdir)
        peer.cardanoSk
      publicPaymentKeyToFile (ops.mkCardanoVk workdir)
        cardanoVk
      privatePaymentKeyToFile (ops.mkWalletSk workdir)
        peer.walletSk
      genHydraKeys $ ops.mkHydra workdir
      pure $ idx /\ workdir

genHydraKeys :: forall m. MonadEffect m => FilePath -> m Unit
genHydraKeys fp =
  liftEffect do
    void $ execSync ("hydra-node gen-hydra-key --output-file " <> fp)
      defaultExecSyncOptions

publishHydraScripts :: forall m. MonadEffect m => FilePath -> FilePath -> m String
publishHydraScripts nodeSocket cardanoSk =
  liftEffect do
    let
      cmd =
        "hydra-node publish-scripts --mainnet --node-socket "
          <> nodeSocket
          <> " --cardano-signing-key "
          <> cardanoSk
    Buffer.toString Encoding.UTF8
      =<< execSync cmd defaultExecSyncOptions

patchContractEnv :: forall (a :: Type). NetworkId -> Contract ContractEnv
patchContractEnv network = do
  pparams <- pparamsSlice
  ask <#> \env -> env
    { networkId = network
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
    caDecodeFile pparamsSliceCodec "protocol-parameters.json"

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
