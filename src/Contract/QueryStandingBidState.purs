module HydraAuctionOffchain.Contract.QueryStandingBidState
  ( QueryStandingBidStateError
      ( QueryBidState_Error_CurrentTimeBeforeBiddingStart
      , QueryBidState_Error_CouldNotFindStandingBidUtxo
      )
  , queryStandingBidState
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.Transaction (TransactionOutput)
import Contract.Value (CurrencySymbol)
import Contract.Value (valueOf) as Value
import Control.Error.Util ((!?))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (find) as Array
import HydraAuctionOffchain.Contract.MintingPolicies (standingBidTokenName)
import HydraAuctionOffchain.Contract.Types
  ( class ToContractError
  , AuctionInfo(AuctionInfo)
  , AuctionTerms(AuctionTerms)
  , ContractOutput
  , StandingBidState
  , mkContractOutput
  )
import HydraAuctionOffchain.Helpers (getInlineDatum, getTxOutsAt)

queryStandingBidState :: AuctionInfo -> Contract (ContractOutput StandingBidState)
queryStandingBidState =
  mkContractOutput identity <<< queryStandingBidStateWithErrors

queryStandingBidStateWithErrors
  :: AuctionInfo
  -> ExceptT QueryStandingBidStateError Contract StandingBidState
queryStandingBidStateWithErrors auctionInfo = do
  let
    AuctionInfo auctionInfoRec = auctionInfo
    auctionCs = auctionInfoRec.auctionId
    AuctionTerms auctionTermsRec = auctionInfoRec.auctionTerms

  -- Check that the query is executed after the bidding start time:
  nowTime <- lift currentTime
  when (nowTime < auctionTermsRec.biddingStart) $
    throwError QueryBidState_Error_CurrentTimeBeforeBiddingStart

  -- Look up standing bid:
  findStandingBid auctionInfoRec.standingBidAddr auctionCs
    !? QueryBidState_Error_CouldNotFindStandingBidUtxo

findStandingBid :: Address -> CurrencySymbol -> Contract (Maybe StandingBidState)
findStandingBid standingBidAddr auctionCs =
  getTxOutsAt standingBidAddr <#>
    (getInlineDatum <=< Array.find hasStandingBidToken)
  where
  hasStandingBidToken :: TransactionOutput -> Boolean
  hasStandingBidToken txOut =
    Value.valueOf (unwrap txOut).amount auctionCs standingBidTokenName == one

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
  toContractError = wrap <<< case _ of
    QueryBidState_Error_CurrentTimeBeforeBiddingStart ->
      { errorCode: "QueryStandingBidState01"
      , message: "Standing bid cannot be queried before bidding start time."
      }
    QueryBidState_Error_CouldNotFindStandingBidUtxo ->
      { errorCode: "QueryStandingBidState02"
      , message: "Could not find standing bid utxo."
      }
