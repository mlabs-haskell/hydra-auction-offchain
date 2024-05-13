module DelegateServer.Contract.QueryAuction
  ( queryAuction
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Transaction (TransactionInput)
import Contract.Utxos (getUtxo)
import Control.Error.Util ((!?), (??))
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Data.Validation.Semigroup (validation)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfoExtended
  , AuctionInfoValidationError
  , AuctionTermsValidationError
  , ContractOutput
  , mkAuctionInfoExtended
  , mkContractOutput
  , validateAuctionInfo
  , validateAuctionTerms
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)
import HydraAuctionOffchain.Helpers (getInlineDatum)

queryAuction :: TransactionInput -> Contract (ContractOutput AuctionInfoExtended)
queryAuction =
  mkContractOutput identity <<< queryAuctionWithErrors

queryAuctionWithErrors
  :: TransactionInput
  -> ExceptT QueryAuctionError Contract AuctionInfoExtended
queryAuctionWithErrors auctionMetadataOref = do
  -- Query auction metadata utxo:
  auctionMetadataTxOut <- getUtxo auctionMetadataOref
    !? QueryAuction_Error_CouldNotQueryAuctionMetadataUtxo

  -- Decode auction info:
  auctionInfo <- getInlineDatum auctionMetadataTxOut
    ?? QueryAuction_Error_CouldNotDecodeAuctionInfoDatum
  let auctionInfoRec@{ auctionId: auctionCs, auctionTerms } = unwrap auctionInfo

  -- Check auction terms:
  validateAuctionTerms auctionTerms #
    validation (throwError <<< QueryAuction_Error_InvalidAuctionTerms) pure

  -- Build validators:
  validators <-
    withExceptT QueryAuction_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  -- Check auction info:
  validateAuctionInfo auctionInfoRec validators #
    validation (throwError <<< QueryAuction_Error_InvalidAuctionInfo) pure

  let auctionInfoExtended = mkAuctionInfoExtended auctionInfo $ Just auctionMetadataOref
  pure auctionInfoExtended

----------------------------------------------------------------------
-- Errors

data QueryAuctionError
  = QueryAuction_Error_CouldNotQueryAuctionMetadataUtxo
  | QueryAuction_Error_CouldNotDecodeAuctionInfoDatum
  | QueryAuction_Error_InvalidAuctionTerms (Array AuctionTermsValidationError)
  | QueryAuction_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError
  | QueryAuction_Error_InvalidAuctionInfo (Array AuctionInfoValidationError)

derive instance Generic QueryAuctionError _
derive instance Eq QueryAuctionError

instance Show QueryAuctionError where
  show = genericShow

instance ToContractError QueryAuctionError where
  toContractError = wrap <<< case _ of
    QueryAuction_Error_CouldNotQueryAuctionMetadataUtxo ->
      { errorCode: "QueryAuction01"
      , message: "Could not query auction metadata utxo."
      }
    QueryAuction_Error_CouldNotDecodeAuctionInfoDatum ->
      { errorCode: "QueryAuction02"
      , message: "Could not decode AuctionInfo."
      }
    QueryAuction_Error_InvalidAuctionTerms errors ->
      { errorCode: "QueryAuction03"
      , message: "Invalid auction terms, errors: " <> show errors <> "."
      }
    QueryAuction_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "QueryAuction04"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
    QueryAuction_Error_InvalidAuctionInfo errors ->
      { errorCode: "QueryAuction05"
      , message: "Invalid auction info, errors: " <> show errors <> "."
      }
