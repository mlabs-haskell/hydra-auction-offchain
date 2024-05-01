module DelegateServer.Handlers.SignCommitTx
  ( CommitTxSignature
  , CommitTxValidationError
      ( CommitLeaderIsVerifierError
      , MissingStandingBidInputError
      , MissingHydraInitialInputError
      , InvalidOtherInputsError
      , MintValueNonEmptyError
      , InvalidCommitOutputError
      )
  , SignCommitTxError
      ( CommitTxDecodingFailed
      , CommitTxCouldNotResolveInputs
      , CommitTxValidationFailed
      , CommitTxSigningFailed
      )
  , SignCommitTxRequestPayload
  , SignCommitTxResponse
  , signCommitTxErrorCodec
  , signCommitTxHandler
  , signCommitTxHandlerImpl
  , signCommitTxRequestPayloadCodec
  , signCommitTxResponseCodec
  ) where

import Prelude

import Contract.Address (PubKeyHash, pubKeyHashAddress, toPubKeyHash, toStakingCredential)
import Contract.Monad (Contract, liftedM)
import Contract.Transaction
  ( FinalizedTransaction(FinalizedTransaction)
  , OutputDatum(NoOutputDatum)
  , Transaction
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , Vkeywitness
  , _body
  , _inputs
  , signTransaction
  )
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, TokenName, mkTokenName)
import Contract.Value (geq, singleton, valueOf) as Value
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Parallel (parTraverse)
import Ctl.Internal.Plutus.Conversion (toPlutusValue)
import Ctl.Internal.Serialization.Hash (ed25519KeyHashToBytes)
import Data.Array (all, difference, find, fromFoldable, length, partition) as Array
import Data.Codec.Argonaut (JsonCodec, array, object, string) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Foldable (and, fold)
import Data.Generic.Rep (class Generic)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Just, Nothing), isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Validation.Semigroup (V, validation)
import Data.Variant (inj, match) as Variant
import DelegateServer.App (runContract)
import DelegateServer.Lib.Transaction (txSignatures)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.State (class AppInit, access, readAppState)
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import Effect.Exception (error)
import Effect.Exception (message) as Error
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Codec (pubKeyHashCodec, txCodec, vkeyWitnessCodec)
import HydraAuctionOffchain.Contract.MintingPolicies (standingBidTokenName)
import HydraAuctionOffchain.Contract.QueryUtxo (isStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types (AuctionInfoRec)
import HydraAuctionOffchain.Helpers (errV, fromJustWithErr)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import Type.Proxy (Proxy(Proxy))

type SignCommitTxRequestPayload =
  { commitTx :: Transaction
  , commitLeader :: PubKeyHash
  }

signCommitTxRequestPayloadCodec :: CA.JsonCodec SignCommitTxRequestPayload
signCommitTxRequestPayloadCodec =
  CA.object "SignCommitTxRequestPayload" $ CAR.record
    { commitTx: txCodec
    , commitLeader: pubKeyHashCodec
    }

type CommitTxSignature = Vkeywitness

type SignCommitTxResponse = ServerResponse CommitTxSignature SignCommitTxError

signCommitTxResponseCodec :: CA.JsonCodec SignCommitTxResponse
signCommitTxResponseCodec = serverResponseCodec vkeyWitnessCodec signCommitTxErrorCodec

signCommitTxHandler :: forall m. AppInit m => String -> m HTTPure.Response
signCommitTxHandler =
  respCreatedOrBadRequest signCommitTxResponseCodec
    <=< signCommitTxHandlerImpl

signCommitTxHandlerImpl :: forall m. AppInit m => String -> m SignCommitTxResponse
signCommitTxHandlerImpl bodyStr = do
  case caDecodeString signCommitTxRequestPayloadCodec bodyStr of
    Left decodeErr ->
      pure $ ServerResponseError $ CommitTxDecodingFailed decodeErr
    Right { commitTx, commitLeader } -> do
      auctionInfo <- unwrap <$> readAppState (Proxy :: _ "auctionInfo")
      hydraHeadCs <- readAppState (Proxy :: _ "headCs")
      { cardanoSk } <- unwrap <$> access (Proxy :: _ "config")
      runContract $ withWallet cardanoSk do
        let commitTxInputs = Array.fromFoldable $ commitTx ^. _body <<< _inputs
        resolvedInputs' <-
          sequence <$>
            parTraverse (\inp -> map (Tuple inp) <$> getUtxo inp)
              commitTxInputs
        case resolvedInputs' of
          Nothing ->
            pure $ ServerResponseError CommitTxCouldNotResolveInputs
          Just resolvedInputs -> do
            verifier <- unwrap <$> liftedM "Could not get verifier pkh" ownPaymentPubKeyHash
            let
              validationParams =
                { commitTx
                , commitLeader
                , verifier
                , auctionInfo
                , hydraHeadCs
                , resolvedInputs
                }
            validateCommitTx validationParams #
              validation (pure <<< ServerResponseError <<< CommitTxValidationFailed) \_ ->
                catchError
                  (map ServerResponseSuccess $ signTxReturnSignature commitTx)
                  ( pure
                      <<< ServerResponseError
                      <<< CommitTxSigningFailed
                      <<< Error.message
                  )

signTxReturnSignature :: Transaction -> Contract CommitTxSignature
signTxReturnSignature tx = do
  let wrappedTx = FinalizedTransaction tx
  signedTx <- unwrap <$> signTransaction wrappedTx
  case Array.difference (txSignatures signedTx) (txSignatures tx) of
    [ signature ] -> pure signature
    _ -> throwError $ error "Could not find signature after signing."

----------------------------------------------------------------------
-- SignCommitTxError

data SignCommitTxError
  = CommitTxDecodingFailed String
  | CommitTxCouldNotResolveInputs
  | CommitTxValidationFailed (Array CommitTxValidationError)
  | CommitTxSigningFailed String

derive instance Generic SignCommitTxError _
derive instance Eq SignCommitTxError

instance Show SignCommitTxError where
  show = genericShow

signCommitTxErrorCodec :: CA.JsonCodec SignCommitTxError
signCommitTxErrorCodec =
  dimap toVariant fromVariant
    ( CAV.variantMatch
        { "CommitTxDecodingFailed": Right CA.string
        , "CommitTxCouldNotResolveInputs": Left unit
        , "CommitTxValidationFailed": Right $ CA.array commitTxValidationErrorCodec
        , "CommitTxSigningFailed": Right CA.string
        }
    )
  where
  toVariant = case _ of
    CommitTxDecodingFailed x ->
      Variant.inj (Proxy :: _ "CommitTxDecodingFailed") x
    CommitTxCouldNotResolveInputs ->
      Variant.inj (Proxy :: _ "CommitTxCouldNotResolveInputs") unit
    CommitTxValidationFailed x ->
      Variant.inj (Proxy :: _ "CommitTxValidationFailed") x
    CommitTxSigningFailed x ->
      Variant.inj (Proxy :: _ "CommitTxSigningFailed") x

  fromVariant = Variant.match
    { "CommitTxDecodingFailed": CommitTxDecodingFailed
    , "CommitTxCouldNotResolveInputs": const CommitTxCouldNotResolveInputs
    , "CommitTxValidationFailed": CommitTxValidationFailed
    , "CommitTxSigningFailed": CommitTxSigningFailed
    }

----------------------------------------------------------------------
-- CommitTx validation
--
-- We need to perform exhaustive validation as if the transaction
-- came from an untrusted source, because we cannot trust an
-- individual delegate, only the delegate group as a whole. 
-- This also eliminates the need for an authenticated endpoint for
-- CommitTx multi-signing.

data CommitTxValidationError
  = CommitLeaderIsVerifierError
  | MissingStandingBidInputError
  | MissingHydraInitialInputError
  | InvalidOtherInputsError
  | MintValueNonEmptyError
  | InvalidCommitOutputError

derive instance Generic CommitTxValidationError _
derive instance Eq CommitTxValidationError

instance Show CommitTxValidationError where
  show = genericShow

commitTxValidationErrorCodec :: CA.JsonCodec CommitTxValidationError
commitTxValidationErrorCodec =
  CAG.nullarySum "CommitTxValidationError"

type CommitTxValidationParams (r :: Row Type) =
  { commitTx :: Transaction
  , commitLeader :: PubKeyHash
  , verifier :: PubKeyHash
  , auctionInfo :: Record (AuctionInfoRec r)
  , hydraHeadCs :: CurrencySymbol
  , resolvedInputs :: Array (TransactionInput /\ TransactionOutput)
  }

validateCommitTx
  :: forall (r :: Row Type)
   . CommitTxValidationParams r
  -> V (Array CommitTxValidationError) Unit
validateCommitTx p = do
  fold
    [ checkCommitLeader `errV` CommitLeaderIsVerifierError
    , checkStandingBidInput `errV` MissingStandingBidInputError
    , checkHydraInitInput `errV` MissingHydraInitialInputError
    , checkOtherInputs `errV` InvalidOtherInputsError
    , checkMint `errV` MintValueNonEmptyError
    , checkCommitOutput `errV` InvalidCommitOutputError
    ]
  where
  txBody = unwrap $ p.commitTx ^. _body

  commitLeaderPTokenName :: TokenName
  commitLeaderPTokenName =
    fromJustWithErr "commitLeaderPTokenName"
      (mkTokenName $ unwrap $ ed25519KeyHashToBytes $ unwrap p.commitLeader)

  standingBidInputPartition =
    Array.partition (isStandingBidUtxo p.auctionInfo <<< snd)
      p.resolvedInputs

  hydraInitInputPartition =
    Array.partition
      ( \(_ /\ TransactionOutput { amount }) ->
          Value.valueOf amount p.hydraHeadCs commitLeaderPTokenName == one
      )
      standingBidInputPartition.no

  checkCommitLeader :: Boolean
  checkCommitLeader = p.commitLeader /= p.verifier

  -- Check that the transaction spends standing bid utxo.
  checkStandingBidInput :: Boolean
  checkStandingBidInput = Array.length standingBidInputPartition.yes == one

  -- Check that the transaction spends utxo containing a Hydra
  -- participation token with valid head id. 
  checkHydraInitInput :: Boolean
  checkHydraInitInput = Array.length hydraInitInputPartition.yes == one

  -- Check that all other inputs come from pkh addresses not belonging
  -- to the verifier and do not contain datums or reference scripts.
  checkOtherInputs :: Boolean
  checkOtherInputs =
    Array.all
      ( \(_ /\ TransactionOutput out) ->
          and
            [ isJust $ toPubKeyHash out.address
            , isNothing $ toStakingCredential out.address
            , out.address /= pubKeyHashAddress (wrap p.verifier) Nothing
            , out.datum == NoOutputDatum
            , isNothing out.referenceScript
            ]
      )
      hydraInitInputPartition.no

  -- Check that no tokens are minted or burned.
  checkMint :: Boolean
  checkMint = isNothing txBody.mint

  -- Check that standing bid and Hydra participation tokens are sent
  -- to the same output.
  -- Hydra Initial validator will check that the participation token
  -- goes to the right Commit validator address.
  checkCommitOutput :: Boolean
  checkCommitOutput =
    isJust $ Array.find
      ( \output ->
          toPlutusValue (unwrap output).amount `Value.geq`
            ( Value.singleton p.auctionInfo.auctionId standingBidTokenName one
                <> Value.singleton p.hydraHeadCs commitLeaderPTokenName one
            )
      )
      txBody.outputs
