module DelegateServer.Contract.PlaceBid where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Control.Monad.Except (ExceptT, throwError, withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Typelevel.Undefined (undefined)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfoExtended
  , AuctionTerms(AuctionTerms)
  , BidTerms
  , ContractResult
  )
import HydraAuctionOffchain.Contract.Validators (MkAuctionValidatorsError, mkAuctionValidators)

placeBidL2ContractWithErrors
  :: AuctionInfoExtended
  -> BidTerms
  -> ExceptT PlaceBidL2ContractError Contract ContractResult
placeBidL2ContractWithErrors auctionInfo _bidTerms = do
  let
    auctionInfoRec = unwrap auctionInfo
    auctionCs = auctionInfoRec.auctionId
    auctionTerms@(AuctionTerms auctionTermsRec) = auctionInfoRec.auctionTerms

  -- Check that the current time is within the bidding period:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError PlaceBidL2_Error_CurrentTimeBeforeBiddingStart
  when (nowTime >= auctionTermsRec.biddingEnd) $
    throwError PlaceBidL2_Error_CurrentTimeAfterBiddingEnd

  -- Build validators:
  -- TODO: pre-build standing bid validator
  _validators <-
    withExceptT PlaceBidL2_Error_CouldNotBuildAuctionValidators $
      mkAuctionValidators auctionCs auctionTerms

  undefined

----------------------------------------------------------------------
-- Errors

data PlaceBidL2ContractError
  = PlaceBidL2_Error_CurrentTimeBeforeBiddingStart
  | PlaceBidL2_Error_CurrentTimeAfterBiddingEnd
  | PlaceBidL2_Error_CouldNotBuildAuctionValidators MkAuctionValidatorsError

derive instance Generic PlaceBidL2ContractError _
derive instance Eq PlaceBidL2ContractError

instance Show PlaceBidL2ContractError where
  show = genericShow

instance ToContractError PlaceBidL2ContractError where
  toContractError = wrap <<< case _ of
    PlaceBidL2_Error_CurrentTimeBeforeBiddingStart ->
      { errorCode: "PlaceBidL201"
      , message: "Tx cannot be submitted before bidding start time."
      }
    PlaceBidL2_Error_CurrentTimeAfterBiddingEnd ->
      { errorCode: "PlaceBidL202"
      , message: "Tx cannot be submitted after bidding end time."
      }
    PlaceBidL2_Error_CouldNotBuildAuctionValidators err ->
      { errorCode: "PlaceBidL203"
      , message: "Could not build auction validators, error: " <> show err <> "."
      }
