module HydraAuctionOffchain.Contract.MoveBid
  ( MoveBidContractError
      ( MoveBid_Error_InvalidAuctionTerms
      , MoveBid_Error_InvalidDelegateInfo
      , MoveBid_Error_CurrentTimeBeforeBiddingStart
      , MoveBid_Error_CurrentTimeAfterBiddingEnd
      , MoveBid_Error_MoveBidRequestServiceError
      )
  , MoveBidContractParams(MoveBidContractParams)
  , moveBidContract
  , mkMoveBidContractWithErrors
  ) where

import Contract.Prelude

import Cardano.Types (NetworkId, ScriptHash)
import Contract.Address (getNetworkId)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Control.Monad.Except (ExceptT(ExceptT), throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Codec.Argonaut (JsonCodec, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Profunctor (wrapIso)
import Data.Validation.Semigroup (validation)
import DelegateServer.Handlers.MoveBid (MoveBidResponse)
import HydraAuctionOffchain.Codec (scriptHashCodec)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionTerms(AuctionTerms)
  , AuctionTermsValidationError
  , ContractOutput
  , DelegateInfo
  , DelegateInfoValidationError
  , auctionTermsCodec
  , delegateInfoCodec
  , mkContractOutput
  , randomHttpServer
  , validateAuctionTerms
  , validateDelegateInfo
  )
import HydraAuctionOffchain.Lib.Codec (class HasJson)
import HydraAuctionOffchain.Service.Common (ServiceError)
import HydraAuctionOffchain.Service.DelegateServer (moveBidRequest)

newtype MoveBidContractParams = MoveBidContractParams
  { auctionCs :: ScriptHash
  , auctionTerms :: AuctionTerms
  , delegateInfo :: DelegateInfo
  }

derive instance Generic MoveBidContractParams _
derive instance Newtype MoveBidContractParams _
derive instance Eq MoveBidContractParams

instance Show MoveBidContractParams where
  show = genericShow

instance HasJson MoveBidContractParams NetworkId where
  jsonCodec network = const (moveBidContractParamsCodec network)

moveBidContractParamsCodec :: NetworkId -> CA.JsonCodec MoveBidContractParams
moveBidContractParamsCodec network =
  wrapIso MoveBidContractParams $ CA.object "MoveBidContractParams" $
    CAR.record
      { auctionCs: scriptHashCodec
      , auctionTerms: auctionTermsCodec network
      , delegateInfo: delegateInfoCodec
      }

moveBidContract :: MoveBidContractParams -> Contract (ContractOutput MoveBidResponse)
moveBidContract =
  mkContractOutput identity
    <<< mkMoveBidContractWithErrors

mkMoveBidContractWithErrors
  :: MoveBidContractParams
  -> ExceptT MoveBidContractError Contract MoveBidResponse
mkMoveBidContractWithErrors (MoveBidContractParams params) = do
  let
    auctionCs = params.auctionCs
    auctionTerms@(AuctionTerms auctionTermsRec) = params.auctionTerms
    delegateInfo = params.delegateInfo

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< MoveBid_Error_InvalidAuctionTerms) pure

  -- Check delegate info:
  validateDelegateInfo delegateInfo auctionTerms #
    validation (throwError <<< MoveBid_Error_InvalidDelegateInfo) pure

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError MoveBid_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError MoveBid_Error_CurrentTimeAfterBiddingEnd

  -- Send `moveBid` request to a randomly picked delegate:
  httpServer <- randomHttpServer delegateInfo
  network <- lift getNetworkId
  withExceptT MoveBid_Error_MoveBidRequestServiceError $ ExceptT $ liftAff $
    moveBidRequest httpServer network auctionCs

----------------------------------------------------------------------
-- Errors

data MoveBidContractError
  = MoveBid_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | MoveBid_Error_InvalidDelegateInfo (Array DelegateInfoValidationError)
  | MoveBid_Error_CurrentTimeBeforeBiddingStart
  | MoveBid_Error_CurrentTimeAfterBiddingEnd
  | MoveBid_Error_MoveBidRequestServiceError ServiceError

derive instance Generic MoveBidContractError _

instance Show MoveBidContractError where
  show = genericShow

instance ToContractError MoveBidContractError where
  errorCodePrefix = const "MoveBid"
  errorMessage = case _ of
    MoveBid_Error_InvalidAuctionTerms validationErrors ->
      "Invalid auction terms, errors: " <> show validationErrors <> "."

    MoveBid_Error_InvalidDelegateInfo validationErrors ->
      "Invalid delegate info, errors: " <> show validationErrors <> "."

    MoveBid_Error_CurrentTimeBeforeBiddingStart ->
      "Tx cannot be submitted before bidding start time."

    MoveBid_Error_CurrentTimeAfterBiddingEnd ->
      "Tx cannot be submitted after bidding end time."

    MoveBid_Error_MoveBidRequestServiceError err ->
      "MoveBid request failed with error: " <> show err <> "."
