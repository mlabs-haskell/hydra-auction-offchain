module DelegateServer.Handlers.SignCommitTx
  ( CommitTxSignature
  , CommitTxValidationError
      ( MissingStandingBidInputError
      )
  , SignCommitTxError
      ( CommitTxDecodingFailed
      , CommitTxValidationFailed
      , CommitTxSigningFailed
      )
  , SignCommitTxResponse
  , signCommitTxErrorCodec
  , signCommitTxHandler
  , signCommitTxHandlerImpl
  , signCommitTxResponseCodec
  ) where

import Prelude

import Contract.Transaction
  ( FinalizedTransaction(FinalizedTransaction)
  , Transaction
  , Vkeywitness
  , signTransaction
  )
import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (difference) as Array
import Data.Codec.Argonaut (JsonCodec, array, string) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Validation.Semigroup (V, validation)
import Data.Variant (inj, match) as Variant
import DelegateServer.App (runContract)
import DelegateServer.Lib.Transaction (txSignatures)
import DelegateServer.Lib.Wallet (withWallet)
import DelegateServer.State (class AppBase, class AppInit, access)
import DelegateServer.Types.ServerResponse
  ( ServerResponse(ServerResponseSuccess, ServerResponseError)
  , respCreatedOrBadRequest
  , serverResponseCodec
  )
import Effect.Exception (error)
import Effect.Exception (message) as Error
import HTTPure (Response) as HTTPure
import HydraAuctionOffchain.Codec (txCodec, vkeyWitnessCodec)
import HydraAuctionOffchain.Lib.Json (caDecodeString)
import Type.Proxy (Proxy(Proxy))

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
  case caDecodeString txCodec bodyStr of
    Left decodeErr ->
      pure $ ServerResponseError $ CommitTxDecodingFailed decodeErr
    Right commitTx ->
      validateCommitTx commitTx #
        validation (pure <<< ServerResponseError <<< CommitTxValidationFailed) \_ ->
          catchError
            (map ServerResponseSuccess $ signCommitTxReturnSignature commitTx)
            ( pure
                <<< ServerResponseError
                <<< CommitTxSigningFailed
                <<< Error.message
            )

signCommitTxReturnSignature :: forall m. AppBase m => Transaction -> m CommitTxSignature
signCommitTxReturnSignature tx = do
  { cardanoSk } <- unwrap <$> access (Proxy :: _ "config")
  let wrappedTx = FinalizedTransaction tx
  signedTx <- unwrap <$> runContract (withWallet cardanoSk (signTransaction wrappedTx))
  case Array.difference (txSignatures signedTx) (txSignatures tx) of
    [ signature ] -> pure signature
    _ -> throwError $ error "Could not find signature after signing."

----------------------------------------------------------------------
-- SignCommitTxError

data SignCommitTxError
  = CommitTxDecodingFailed String
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
        , "CommitTxValidationFailed": Right $ CA.array commitTxValidationErrorCodec
        , "CommitTxSigningFailed": Right CA.string
        }
    )
  where
  toVariant = case _ of
    CommitTxDecodingFailed x ->
      Variant.inj (Proxy :: _ "CommitTxDecodingFailed") x
    CommitTxValidationFailed x ->
      Variant.inj (Proxy :: _ "CommitTxValidationFailed") x
    CommitTxSigningFailed x ->
      Variant.inj (Proxy :: _ "CommitTxSigningFailed") x

  fromVariant = Variant.match
    { "CommitTxDecodingFailed": CommitTxDecodingFailed
    , "CommitTxValidationFailed": CommitTxValidationFailed
    , "CommitTxSigningFailed": CommitTxSigningFailed
    }

----------------------------------------------------------------------
-- CommitTx validation

data CommitTxValidationError = MissingStandingBidInputError

derive instance Generic CommitTxValidationError _
derive instance Eq CommitTxValidationError

instance Show CommitTxValidationError where
  show = genericShow

commitTxValidationErrorCodec :: CA.JsonCodec CommitTxValidationError
commitTxValidationErrorCodec =
  CAG.nullarySum "CommitTxValidationError"

-- TODO: perform validation
validateCommitTx :: Transaction -> V (Array CommitTxValidationError) Unit
validateCommitTx _tx = pure unit
