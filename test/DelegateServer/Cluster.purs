module Test.DelegateServer.Cluster
  ( TestAppHandle
  , withWallets'
  ) where

import Prelude

import Contract.Config (mkCtlBackendParams)
import Contract.Monad (Contract, liftContractM)
import Contract.Test (class UtxoDistribution, ContractTest(ContractTest))
import Contract.Test.Plutip (PlutipConfig)
import Contract.Transaction (TransactionInput)
import Contract.Wallet (PrivatePaymentKey)
import Contract.Wallet.Key (publicKeyFromPrivateKey)
import Contract.Wallet.KeyFile (privatePaymentKeyToFile)
import Control.Monad.Except (ExceptT, mapExceptT, throwError)
import Control.Parallel (parTraverse, parTraverse_)
import Ctl.Internal.Helpers (concatPaths, (<</>>))
import Ctl.Internal.Plutip.Types (ClusterStartupParameters)
import Ctl.Internal.Plutip.Utils (tmpdir)
import Data.Array (concat, deleteAt, replicate)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, head, toArray) as NEArray
import Data.Foldable (length)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap, wrap)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Data.UUID (genUUID, toString) as UUID
import DelegateServer.App (runApp)
import DelegateServer.Config (AppConfig, Network(Mainnet))
import DelegateServer.Contract.PlaceBid (PlaceBidL2ContractError, placeBidL2)
import DelegateServer.Main (AppHandle, startDelegateServer)
import DelegateServer.State (access)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.AVar (tryRead) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import HydraAuctionOffchain.Contract.Types (AuctionInfoExtended, BidTerms)
import HydraAuctionOffchain.Helpers (fromJustWithErr)
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
  , randomElem
  )
import Test.Plutip.Config (plutipConfig)
import Test.QuickCheck.Gen (chooseInt, randomSampleOne)
import Type.Proxy (Proxy(Proxy))
import URI.Port (unsafeFromInt) as Port

type TestAppHandle =
  { placeBidL2 :: BidTerms -> ExceptT PlaceBidL2ContractError Aff Unit
  , getActiveAuction :: Aff (Maybe AuctionInfoExtended)
  }

withWallets'
  :: forall (distr :: Type) (wallets :: Type)
   . UtxoDistribution distr wallets
  => distr
  -> ( wallets
       -> (AuctionInfoExtended -> (TestAppHandle -> Aff Unit) -> Contract Unit)
       -> Contract Unit
     )
  -> ContractTest
withWallets' distr tests =
  ContractTest \h ->
    let
      numDelegates =
        unsafePerformEffect $ randomSampleOne $ chooseInt 1 5
      distrDelegates =
        concat $ replicate numDelegates [ defDistribution, defDistribution ]
    in
      h (distr /\ distrDelegates) \mPlutipClusterParams (wallets /\ delegateWallets) -> do
        plutipClusterParams <-
          liftContractM "Could not get Plutip cluster params"
            mPlutipClusterParams
        delegateWalletsGrouped <-
          liftContractM "Expected even number of delegate wallets"
            (chunksOf2 delegateWallets)
        tests wallets \auctionInfo action -> do
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
          liftAff $ withDelegateServerCluster clusterConfig peers action

withDelegateServerCluster
  :: DelegateServerClusterConfig
  -> NonEmptyArray DelegateServerPeer
  -> (TestAppHandle -> Aff Unit)
  -> Aff Unit
withDelegateServerCluster clusterConfig peers action =
  bracket
    (startDelegateServerCluster clusterConfig peers)
    ( \(appHandles /\ workdirCleanupHandler) ->
        parTraverse_ (liftEffect <<< _.cleanupHandler) appHandles
          *> liftEffect workdirCleanupHandler
    )
    ( \(apps /\ _) -> action
        { placeBidL2:
            \bidTerms -> do
              appHandle <- randomElem apps
              mapExceptT (runApp appHandle.appState) $ placeBidL2 appHandle.ws bidTerms

        , getActiveAuction: do
            appHandle <- randomElem apps
            runApp appHandle.appState $
              liftAff <<< AVar.tryRead =<< access (Proxy :: _ "auctionInfo")
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
    , mkHydraNode: \idx -> { host: localhost, port: UInt.fromInt (7060 + idx) }
    , mkHydraNodeApi: \idx -> { host: localhost, port: UInt.fromInt (7080 + idx) }
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
