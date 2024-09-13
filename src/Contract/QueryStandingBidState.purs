module HydraAuctionOffchain.Contract.QueryStandingBidState
  ( QueryStandingBidStateError
      ( QueryBidState_Error_CurrentTimeBeforeBiddingStart
      , QueryBidState_Error_CouldNotFindStandingBidUtxo
      )
  , queryStandingBidState
  ) where

import Contract.Prelude

import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Control.Error.Util ((!?))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import HydraAuctionOffchain.Contract.QueryUtxo (queryStandingBidUtxo)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfoExtended(AuctionInfoExtended)
  , AuctionTerms(AuctionTerms)
  , ContractOutput
  , StandingBidState
  , mkContractOutput
  )

queryStandingBidState :: AuctionInfoExtended -> Contract (ContractOutput StandingBidState)
queryStandingBidState =
  mkContractOutput identity <<< queryStandingBidStateWithErrors

queryStandingBidStateWithErrors
  :: AuctionInfoExtended
  -> ExceptT QueryStandingBidStateError Contract StandingBidState
queryStandingBidStateWithErrors auctionInfo = do
  let
    AuctionInfoExtended auctionInfoRec = auctionInfo
    AuctionTerms auctionTermsRec = auctionInfoRec.auctionTerms

  -- Check that the query is executed after the bidding start time:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError QueryBidState_Error_CurrentTimeBeforeBiddingStart

  -- Look up standing bid:
  snd <$> queryStandingBidUtxo auctionInfoRec !?
    QueryBidState_Error_CouldNotFindStandingBidUtxo

----------------------------------------------------------------------
-- Errors

data QueryStandingBidStateError
  = QueryBidState_Error_CurrentTimeBeforeBiddingStart
  | QueryBidState_Error_CouldNotFindStandingBidUtxo

derive instance Generic QueryStandingBidStateError _
derive instance Eq QueryStandingBidStateError

instance Show QueryStandingBidStateError where
  show = genericShow

instance ToContractError QueryStandingBidStateError where
  errorCodePrefix = const "QueryStandingBidState"
  errorMessage = case _ of
    QueryBidState_Error_CurrentTimeBeforeBiddingStart ->
      "Standing bid cannot be queried before bidding start time."

    QueryBidState_Error_CouldNotFindStandingBidUtxo ->
      "Could not find standing bid utxo."
