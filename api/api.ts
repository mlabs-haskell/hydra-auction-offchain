import * as Purs from "../output/HydraAuctionOffchain.Api/index.js";
import unimplemented from "ts-unimplemented";

import type {
  AnnounceAuctionContractParams,
  AnnounceAuctionContractOutput,
  AuthorizeBiddersContractParams,
  AuctionInfo,
  BidderInfoCandidate,
  BigInt,
  ByteArray,
  ContractOutput,
  CurrencySymbol,
  DiscoverSellerSigContractParams,
  EnterAuctionContractParams,
  PlaceBidContractParams,
  PubKeyHash,
  StartBiddingContractParams,
  StandingBidState,
  TokenName,
  TransactionHash,
  TxCbor,
  VerificationKey,
  WalletApp
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
  walletApp: WalletApp | null
): Promise<Array<AuctionInfo>> => Purs.queryAuctions(walletApp)();

export const cleanupAuction = async (
  auctionInfo: AuctionInfo
): Promise<ContractOutput<TransactionHash>> => unimplemented();

export const queryStandingBidState = async (
  walletApp: WalletApp | null,
  auctionInfo: AuctionInfo
): Promise<StandingBidState> => Purs.queryStandingBidState(walletApp)(auctionInfo)();

// Auctions (seller) -------------------------------------------------

/**
 * Announce auction by declaring the auction metadata and placing the auction
 * lot into the escrow.
 */
export const announceAuction = async (
  walletApp: WalletApp,
  params: AnnounceAuctionContractParams
): Promise<ContractOutput<AnnounceAuctionContractOutput>> =>
  Purs.announceAuction(walletApp)(params)();

/**
 * Discover bidders who have indicated their interest in participating
 * in the auction by paying a bidder deposit.
 */
export const discoverBidders = async (
  walletApp: WalletApp | null,
  auctionInfo: AuctionInfo
): Promise<Array<BidderInfoCandidate>> => Purs.discoverBidders(walletApp)(auctionInfo)();

/**
 * Authorize bidders to participate in the auction by posting a list of
 * signatures onchain at the personal oracle validator. Bidders can then
 * discover these signatures using `discoverSellerSignatures`.
 */
export const authorizeBidders = async (
  walletApp: WalletApp,
  params: AuthorizeBiddersContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.authorizeBidders(walletApp)(params)();

/**
 * Initiate the auction, enabling bidders to place their bids.
 */
export const startBidding = async (
  walletApp: WalletApp,
  params: StartBiddingContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.startBidding(walletApp)(params)();

/**
 * Claim the auction lot and deposit if the auction lot has not been purchased
 * by the winning bidder before purchase deadline, distribute the auction fees
 * to the delegates.
 */
export const claimAuctionLotSeller = async (
  auctionInfo: AuctionInfo
): Promise<ContractOutput<TransactionHash>> => unimplemented();

// Auctions (bidder) -------------------------------------------------

/**
 * Apply for participation in the auction by sending a deposit equal to or
 * greater than the `minDepositAmount` specified in `AuctionTerms`.
 *
 * If `depositAmount` is set to `null`, the minimum deposit will be made.
 */
export const enterAuction = async (
  walletApp: WalletApp,
  params: EnterAuctionContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.enterAuction(walletApp)(params)();

/**
 * Discover own bidder-auction authorization signature required for
 * placing bids on a particular auction.
 */
export const discoverSellerSignature = async (
  walletApp: WalletApp,
  params: DiscoverSellerSigContractParams
): Promise<ContractOutput<ByteArray | null>> =>
  Purs.discoverSellerSignature(walletApp)(params)();

export const placeBid = async (
  walletApp: WalletApp,
  params: PlaceBidContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.placeBid(walletApp)(params)();

/**
 * Claim the auction lot if the bid placed by the bidder wins, distribute the
 * auction fees to the delegates.
 */
export const claimAuctionLotBidder = async (
  auctionInfo: AuctionInfo
): Promise<ContractOutput<TransactionHash>> => unimplemented();

/**
 * Reclaim the deposit if someone's else bid wins.
 */
export const claimDepositLoser = async (
  auctionInfo: AuctionInfo
): Promise<ContractOutput<TransactionHash>> => unimplemented();

// Auctions (delegate) -----------------------------------------------

export const announceDelegateGroup = async (
  groupName: string,
  groupUrl: string,
  delegates: Array<PubKeyHash>
): Promise<ContractOutput<TxCbor>> => unimplemented();

// Helpers -----------------------------------------------------------

export const awaitTxConfirmed = async (
  walletApp: WalletApp | null,
  txHash: TransactionHash
): Promise<void> => Purs.awaitTxConfirmed(walletApp)(txHash)();

export const mintTokenUsingAlwaysMints = async (
  walletApp: WalletApp,
  tokenName: TokenName,
  quantity: BigInt
): Promise<TransactionHash> =>
  Purs.mintTokenUsingAlwaysMints(walletApp)(tokenName)(quantity)();
