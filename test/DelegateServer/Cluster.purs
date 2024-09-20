module Test.DelegateServer.Cluster
  ( TestAppHandle
  , withWallets'
  ) where

import Prelude

import Cardano.Types
  ( BigNum
  , Ed25519KeyHash
  , Language(PlutusV2)
  , NetworkId(MainnetId)
  , TransactionInput
  )
import Cardano.Types.Int (fromInt) as Cardano.Int
import Cardano.Types.PublicKey (hash) as PublicKey
import Cardano.Wallet.Key (KeyWallet(KeyWallet))
import Contract.Config (QueryBackendParams, mkCtlBackendParams)
import Contract.Monad (Contract, ContractEnv, liftContractM, liftedE, runContractInEnv)
import Contract.Test (class UtxoDistribution, ContractTest(ContractTest))
import Contract.Test.Testnet (TestnetConfig)
import Contract.Wallet (PrivatePaymentKey)
import Contract.Wallet.Key (publicKeyFromPrivateKey)
import Contract.Wallet.KeyFile (privatePaymentKeyToFile)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, local)
import Control.Parallel (parTraverse, parTraverse_)
import Ctl.Internal.Contract.Hooks (ClusterParameters)
import Ctl.Internal.Helpers (concatPaths, (<</>>))
import Ctl.Internal.Testnet.Utils (tmpdir)
import Data.Array (concat, deleteAt, replicate)
import Data.Array (fromFoldable) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, head, toArray) as NEArray
import Data.Codec.Argonaut (JsonCodec, array, int, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Foldable (length)
import Data.Int (decimal, toStringAs)
import Data.Log.Level (LogLevel(Info, Warn))
import Data.Map (singleton, values) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (modify, unwrap, wrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Data.UUID (genUUID, toString) as UUID
import DelegateServer.App (AppM, runApp, runContract)
import DelegateServer.Config (AppConfig', Network(Testnet), AuctionSlotConfig)
import DelegateServer.Contract.StandingBid (queryStandingBidL2)
import DelegateServer.Handlers.MoveBid (MoveBidResponse, moveBidHandlerImpl)
import DelegateServer.Handlers.PlaceBid (PlaceBidResponse, placeBidHandlerImpl)
import DelegateServer.HydraNodeApi.WebSocket (HydraNodeApiWebSocket)
import DelegateServer.Main (AppHandle, startDelegateServer)
import DelegateServer.State (access, readAppState)
import DelegateServer.Types.HydraHeadStatus (HydraHeadStatus)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.AVar (read, tryRead) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import Effect.Unsafe (unsafePerformEffect)
import HydraAuctionOffchain.Codec (bigNumCodec)
import HydraAuctionOffchain.Contract.Types
  ( AuctionInfoExtended
  , BidTerms
  , StandingBidState
  , bidTermsCodec
  )
import HydraAuctionOffchain.Helpers (fromJustWithErr, randomElem)
import HydraAuctionOffchain.Lib.Json (caDecodeFile, caEncodeString)
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
  , unsafeHead
  )
import Test.Localnet.Config (localnetConfig)
import Test.QuickCheck.Gen (chooseInt, randomSampleOne)
import Type.Proxy (Proxy(Proxy))
import URI.Port (toInt, unsafeFromInt) as Port

type TestAppHandle =
  { getActiveAuction :: Contract (Maybe AuctionInfoExtended)
  , getHeadStatus :: Contract HydraHeadStatus
  , moveBidToL2 :: Contract MoveBidResponse
  , queryStandingBidL2 :: Contract (Maybe StandingBidState)
  , placeBidL2 :: BidTerms -> Contract PlaceBidResponse
  }

withWallets'
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => Ref ClusterParameters
  -> distr
  -> ( wallets
       -> Array Ed25519KeyHash
       -> (AuctionInfoExtended -> (TestAppHandle -> Contract Unit) -> Contract Unit)
       -> Contract Unit
     )
  -> ContractTest
withWallets' localnetClusterParamsRef distr tests =
  ContractTest \h ->
    let
      numDelegates = unsafePerformEffect $ randomSampleOne $ chooseInt 1 3
      distrDelegates =
        concat $ replicate numDelegates [ defDistribution, defDistribution ]
    in
      h (distr /\ distrDelegates) \(wallets /\ delegateWallets) ->
        do
          contractEnv <- ask
          delegateWalletsGrouped <-
            liftContractM "Expected even number of delegate wallets"
              (chunksOf2 delegateWallets)
          delegates <-
            liftAff $ traverse
              ( \((KeyWallet kw) /\ _) ->
                  PublicKey.hash <<< publicKeyFromPrivateKey <<< unwrap <$> kw.paymentKey
              )
              delegateWalletsGrouped
          tests wallets delegates \auctionInfo action -> do
            auctionMetadataOref <-
              liftContractM "Could not get auction metadata oref"
                (unwrap auctionInfo).metadataOref
            localnetClusterParams <- liftEffect $ Ref.read localnetClusterParamsRef
            let
              clusterConfig =
                { auctionMetadataOref
                , localnetClusterParams
                , localnetConfig
                }
            peers <-
              liftAff $ traverse
                ( \(cardanoKw /\ walletKw) ->
                    { cardanoSk: _, walletSk: _ } <$> (unwrap cardanoKw).paymentKey
                      <*> (unwrap walletKw).paymentKey
                )
                delegateWalletsGrouped
            liftAff $ withDelegateServerCluster
              contractEnv
              clusterConfig
              (fromJustWithErr "withWallets'" $ NEArray.fromArray peers)
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
          withRandomApp :: forall a. (HydraNodeApiWebSocket -> AppM a) -> Contract a
          withRandomApp f =
            liftAff do
              appHandle <- randomElem apps
              appManager <- liftAff $ unwrap <$> AVar.read appHandle.appManager
              let
                { appState, appLogger, hydraNodeApiWs } =
                  unsafeHead $ Array.fromFoldable $ Map.values
                    appManager.activeAuctions
              runApp appState appLogger $ f hydraNodeApiWs

          withRandomApp_ :: forall a. AppM a -> Contract a
          withRandomApp_ = withRandomApp <<< const
        in
          runContractInEnv contractEnv $ action
            { getActiveAuction:
                withRandomApp_ $
                  liftAff <<< AVar.tryRead =<< access (Proxy :: _ "auctionInfo")

            , getHeadStatus:
                withRandomApp_ $ readAppState (Proxy :: _ "headStatus")

            , moveBidToL2:
                withRandomApp moveBidHandlerImpl

            , queryStandingBidL2:
                withRandomApp_ $ map snd <$> queryStandingBidL2

            , placeBidL2:
                \bidTerms ->
                  withRandomApp \hydraNodeApiWs -> do
                    env <- wrap <$> runContract (patchContractEnv MainnetId)
                    let body = caEncodeString (bidTermsCodec MainnetId) bidTerms
                    local (_ { contractEnv = env }) $ placeBidHandlerImpl hydraNodeApiWs body
            }
    )

type DelegateServerPeer =
  { cardanoSk :: PrivatePaymentKey
  , walletSk :: PrivatePaymentKey
  }

type DelegateServerClusterConfig =
  { auctionMetadataOref :: TransactionInput
  , localnetClusterParams :: ClusterParameters
  , localnetConfig :: TestnetConfig
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
  -> Aff (Array (AppConfig' (Array (AuctionSlotConfig Maybe)) QueryBackendParams))
genDelegateServerConfigs clusterWorkdir clusterConfig peers = do
  peers' <- createWorkdirsStoreKeys
  hydraScriptsTxHash <- publishHydraScripts
    clusterConfig.localnetClusterParams.nodeSocketPath
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
    }

  worker
    :: Array (Int /\ FilePath)
    -> String
    -> Array (AppConfig' (Array (AuctionSlotConfig Maybe)) QueryBackendParams)
  worker peers' hydraScriptsTxHash =
    peers' <#> \(idx /\ workdir) -> wrap
      { auctionConfig:
          [ { auctionMetadataOref: Just clusterConfig.auctionMetadataOref
            , hydraNodeId: toStringAs decimal idx
            , hydraNode: ops.mkHydraNode idx
            , hydraNodeApi: ops.mkHydraNodeApi idx
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
            , cardanoSk: ops.mkCardanoSk workdir
            }
          ]
      , serverPort: ops.mkServerPort idx
      , wsServerPort: ops.mkWsServerPort idx
      , hydraPersistDir: ops.mkPersistDir workdir
      , hydraSk: ops.mkHydraSk workdir
      , nodeSocket: clusterConfig.localnetClusterParams.nodeSocketPath
      , network: Testnet { magic: localnetConfig.clusterConfig.testnetMagic }
      , queryBackend:
          mkCtlBackendParams
            { ogmiosConfig:
                clusterConfig.localnetConfig.ogmiosConfig
            , kupoConfig:
                clusterConfig.localnetConfig.kupoConfig
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
        "hydra-node publish-scripts --testnet-magic 2 --node-socket "
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
                    Map.singleton PlutusV2
                      (wrap $ Cardano.Int.fromInt <$> pparams.costModels."PlutusV2")
                , maxTxExUnits = wrap
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
  { maxTxExecutionUnits :: { memory :: BigNum, steps :: BigNum }
  , costModels :: { "PlutusV2" :: Array Int }
  }

pparamsSliceCodec :: CA.JsonCodec PParamsSlice
pparamsSliceCodec =
  CA.object "PParamsSlice" $ CAR.record
    { maxTxExecutionUnits:
        CA.object "PParamsSlice:ExUnits" $ CAR.record
          { memory: bigNumCodec
          , steps: bigNumCodec
          }
    , costModels:
        CA.object "PParamsSlice:CostModels" $ CAR.record
          { "PlutusV2": CA.array CA.int
          }
    }
