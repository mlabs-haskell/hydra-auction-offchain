import * as Purs from "../output/HydraAuctionOffchain.Api/index.js";
import unimplemented from "ts-unimplemented";

import type {
  AnnounceAuctionContractParams,
  AnnounceAuctionContractOutput,
  AuthorizeBiddersContractParams,
  AuctionFilters,
  AuctionInfo,
  BidderInfoCandidate,
  BigInt,
  ByteArray,
  ContractConfig,
  ContractOutput,
  CurrencySymbol,
  DiscoverSellerSigContractParams,
  EnterAuctionContractParams,
  MoveBidL2ContractParams,
  PlaceBidContractParams,
  PlaceBidL2ContractParams,
  PubKeyHash,
  StartBiddingContractParams,
  StandingBidState,
  TokenName,
  TransactionHash,
  TxCbor,
  VerificationKey
} from "./types";

// High-level auction workflow pseudocode for L1:
// NOTE: uppercase functions should be implemented on frontend
//
// 01. seller: auctionTerms <- BUILD_AUCTION_TERMS()
// 02. seller: auctionInfo <- announceAuction(auctionTerms)
// 03. bidder: auctionInfoArr <- queryAuctions()
// 04. bidder: auctionInfo = SELECT_AUCTION(auctionInfoArr)
// 05. bidder: depositAmount <- SELECT_DEPOSIT_AMOUNT(auctionInfo)
// 06. bidder: enterAuction(auctionInfo, depositAmount)
// 07. seller: bidders <- discoverBidders(auctionInfo)
// 08. seller: biddersToAuthorize <- SELECT_BIDDERS(bidders)
// 09. seller: authorizeBidders(auctionInfo.auctionId, biddersToAuthorize)
// 10. bidder: sellerSignature <- discoverSellerSignature(auctionInfo.auctionId, auctionInfo.auctionTerms.sellerAddress)
// 11. seller: startBidding(auctionInfo)
// 12. bidder: bidState <- queryStandingBidState(auctionInfo)
// 13. bidder: bidAmount <- SELECT_BID_AMOUNT(auctionInfo, bidState.price)
// 14. bidder: placeBid(auctionInfo, sellerSignature, bidAmount)
// 15. bidder(winner): claimAuctionLotBidder(auctionInfo)
// 16. bidder(loser): claimDepositLoser(auctionInfo)
// 17. seller(winner didn't buy): claimAuctionLotSeller(auctionInfo)
// 18. anyone: cleanupAuction(auctionInfo)

// Auctions (anyone) -------------------------------------------------

export const queryAuctions = async (
  config: ContractConfig,
  filters: AuctionFilters = {}
): Promise<Array<AuctionInfo>> => Purs.queryAuctions(config)(filters)();

/* NOTE: not implemented, returns stubbed data */
export const cleanupAuction = async (
  config: ContractConfig,
  auctionInfo: AuctionInfo
): Promise<ContractOutput<TransactionHash>> => Purs.cleanupAuction(config)(auctionInfo)();

export const queryStandingBidState = async (
  config: ContractConfig,
  auctionInfo: AuctionInfo
): Promise<ContractOutput<StandingBidState>> =>
  Purs.queryStandingBidState(config)(auctionInfo)();

export const moveBidL2 = async (
  config: ContractConfig,
  params: MoveBidL2ContractParams
): Promise<ContractOutput<any>> => Purs.moveBidL2(config)(params)();

// Auctions (seller) -------------------------------------------------

/**
 * Announce auction by declaring the auction metadata and placing the auction
 * lot into the escrow.
 */
export const announceAuction = async (
  config: ContractConfig,
  params: AnnounceAuctionContractParams
): Promise<ContractOutput<AnnounceAuctionContractOutput>> =>
  Purs.announceAuction(config)(params)();

/**
 * Discover bidders who have indicated their interest in participating
 * in the auction by paying a bidder deposit.
 */
export const discoverBidders = async (
  config: ContractConfig,
  auctionInfo: AuctionInfo
): Promise<Array<BidderInfoCandidate>> => Purs.discoverBidders(config)(auctionInfo)();

/**
 * Authorize bidders to participate in the auction by posting a list of
 * signatures onchain at the personal oracle validator. Bidders can then
 * discover these signatures using `discoverSellerSignatures`.
 */
export const authorizeBidders = async (
  config: ContractConfig,
  params: AuthorizeBiddersContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.authorizeBidders(config)(params)();

/**
 * Initiate the auction, enabling bidders to place their bids.
 */
export const startBidding = async (
  config: ContractConfig,
  params: StartBiddingContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.startBidding(config)(params)();

/**
 * Claim the auction lot and deposit if the auction lot has not been purchased
 * by the winning bidder before purchase deadline, distribute the auction fees
 * to the delegates.
 */
export const claimAuctionLotSeller = async (
  config: ContractConfig,
  auctionInfo: AuctionInfo
): Promise<ContractOutput<TransactionHash>> =>
  Purs.claimAuctionLotSeller(config)(auctionInfo)();

// Auctions (bidder) -------------------------------------------------

/**
 * Apply for participation in the auction by sending a deposit equal to or
 * greater than the `minDepositAmount` specified in `AuctionTerms`.
 *
 * If `depositAmount` is set to `null`, the minimum deposit will be made.
 */
export const enterAuction = async (
  config: ContractConfig,
  params: EnterAuctionContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.enterAuction(config)(params)();

/**
 * Discover own bidder-auction authorization signature required for
 * placing bids on a particular auction.
 */
export const discoverSellerSignature = async (
  config: ContractConfig,
  params: DiscoverSellerSigContractParams
): Promise<ContractOutput<ByteArray | null>> => Purs.discoverSellerSignature(config)(params)();

export const placeBid = async (
  config: ContractConfig,
  params: PlaceBidContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.placeBid(config)(params)();

export const placeBidL2 = async (
  config: ContractConfig,
  params: PlaceBidL2ContractParams
): Promise<ContractOutput<any>> => Purs.placeBidL2(config)(params)();

/**
 * Claim the auction lot if the bid placed by the bidder wins, distribute the
 * auction fees to the delegates.
 */
export const claimAuctionLotBidder = async (
  config: ContractConfig,
  auctionInfo: AuctionInfo
): Promise<ContractOutput<TransactionHash>> =>
  Purs.claimAuctionLotBidder(config)(auctionInfo)();

/**
 * Reclaim the deposit if someone's else bid wins.
 *
 * NOTE: not implemented, returns stubbed data
 */
export const claimDepositLoser = async (
  config: ContractConfig,
  auctionInfo: AuctionInfo
): Promise<ContractOutput<TransactionHash>> => Purs.claimDepositLoser(config)(auctionInfo)();

// Auctions (delegate) -----------------------------------------------

export const announceDelegateGroup = async (
  groupName: string,
  groupUrl: string,
  delegates: Array<PubKeyHash>
): Promise<ContractOutput<TxCbor>> => unimplemented();

// Helpers -----------------------------------------------------------

export const getWalletVk = async (
  config: ContractConfig
): Promise<ContractOutput<VerificationKey>> => Purs.getWalletVk(config)();

export const awaitTxConfirmed = async (
  config: ContractConfig,
  txHash: TransactionHash
): Promise<void> => Purs.awaitTxConfirmed(config)(txHash)();

export const mintTokenUsingAlwaysMints = async (
  config: ContractConfig,
  tokenName: TokenName,
  quantity: BigInt
): Promise<TransactionHash> => Purs.mintTokenUsingAlwaysMints(config)(tokenName)(quantity)();
