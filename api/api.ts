import * as Purs from "../output/HydraAuctionOffchain.Api/index.js";
import unimplemented from "ts-unimplemented";

import type {
  AnnounceAuctionContractParams,
  AnnounceAuctionContractOutput,
  AuthorizeBiddersContractParams,
  AuctionInfo,
  BidderInfo,
  BigInt,
  ByteArray,
  ContractOutput,
  CurrencySymbol,
  DiscoverSellerSigContractParams,
  DiscoverSellerSigContractOutput,
  EnterAuctionContractParams,
  PubKeyHash,
  StartBiddingContractParams,
  TokenName,
  TransactionHash,
  TxCbor,
  VerificationKey,
  WalletApp
} from "./types";

// L1 Auction workflow high-level pseudocode:
// NOTE: uppercase functions should be implemented on frontend
//
// seller: auctionInfo <- announceAuction(auctionTerms)
// bidder: auctionInfoArr <- queryAuctions()
// bidder: auctionInfo = SELECT_AUCTION(auctionInfoArr)
// bidder: depositAmount <- SELECT_DEPOSIT_AMOUNT(auctionInfo)
// bidder: enterAuction(auctionInfo, depositAmount)
// seller: bidders <- discoverBidders(auctionInfo)
// seller: biddersToAuthorize <- SELECT_BIDDERS(bidders)
// seller: authorizeBidders(auctionInfo.auctionId, biddersToAuthorize)
// TODO: bidder: sellerSig <- discoverSellerSignature(auctionInfo.auctionId, auctionInfo.auctionTerms.sellerPkh)
// TODO: ...
//

// Auctions (anyone) -------------------------------------------------

export const queryAuctions = async (
  walletApp: WalletApp | null
): Promise<Array<AuctionInfo>> => Purs.queryAuctions(walletApp)();

export const cleanupAuction = async (
  auctionCs: CurrencySymbol
): Promise<ContractOutput<TransactionHash>> => unimplemented();

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
 *
 * NOTE: not implemented, returns stubbed data
 */
export const discoverBidders = async (
  walletApp: WalletApp | null,
  auctionInfo: AuctionInfo
): Promise<Array<BidderInfo>> => Purs.discoverBidders(walletApp)(auctionInfo)();

/**
 * Authorize bidders to participate in the auction by posting a list of
 * signatures onchain at the personal oracle validator. Bidders can then
 * discover these signatures using `discoverSellerSignatures`.
 *
 * NOTE: not implemented, returns stubbed data
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
  auctionCs: CurrencySymbol
): Promise<ContractOutput<TransactionHash>> => unimplemented();

// Auctions (bidder) -------------------------------------------------

/**
 * Apply for participation in the auction by sending a deposit equal to or
 * greater than the `minDepositAmount` specified in `AuctionTerms`.
 *
 * If `depositAmount` is set to `null`, the minimum deposit will be made.
 *
 * NOTE: not implemented, returns stubbed data
 */
export const enterAuction = async (
  walletApp: WalletApp,
  params: EnterAuctionContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.enterAuction(walletApp)(params)();

/**
 * Discover bidder-auction authorization signature required for
 * placing bids on a particular auction.
 *
 * NOTE: not implemented, returns stubbed data
 */
export const discoverSellerSignature = async (
  walletApp: WalletApp,
  params: DiscoverSellerSigContractParams
): Promise<ContractOutput<DiscoverSellerSigContractOutput>> =>
  Purs.discoverSellerSignature(walletApp)(params)();

export const placeBid = async (
  auctionCs: CurrencySymbol,
  bidAmount: BigInt,
  sellerSignature: ByteArray
): Promise<ContractOutput<TransactionHash>> => unimplemented();

/**
 * Claim the auction lot if the bid placed by the bidder wins, distribute the
 * auction fees to the delegates.
 */
export const claimAuctionLotBidder = async (
  auctionCs: CurrencySymbol
): Promise<ContractOutput<TransactionHash>> => unimplemented();

/**
 * Reclaim the deposit if someone's else bid wins.
 */
export const claimDepositLoser = async (
  auctionCs: CurrencySymbol
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
