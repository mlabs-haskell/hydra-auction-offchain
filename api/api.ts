import * as Purs from "../output/HydraAuctionOffchain.Api/index.js";
import unimplemented from "ts-unimplemented";

import type {
  AnnounceAuctionContractParams,
  AuctionInfo,
  BigInt,
  ByteArray,
  ContractOutput,
  CurrencySymbol,
  PubKeyHash,
  TokenName,
  TransactionHash,
  TxCbor,
  VerificationKey,
  WalletApp
} from "./types";

// Auctions (seller) -----------------------------------------------------------

/**
 * Announce auction by declaring the auction metadata and placing the auction
 * lot into the escrow.
 */
export const announceAuction = async (
  walletApp: WalletApp,
  params: AnnounceAuctionContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.announceAuction(walletApp)(params)();

export const discoverBidders = async (
  walletApp: WalletApp | null
): Promise<Array<VerificationKey>> => unimplemented();

/**
 * Authorize bidders to participate in the auction by posting a list of
 * signatures onchain at the personal oracle validator. Bidders can then
 * discover these signatures using `discoverSellerSignature`.
 */
export const authorizeBidders = async (
  auctionCs: CurrencySymbol,
  biddersToAuthorize: Array<VerificationKey>
): Promise<ContractOutput<TransactionHash>> => unimplemented();

/**
 * Initiate the auction, enabling bidders to place their bids.
 */
export const startBidding = async (
  auctionCs: CurrencySymbol
): Promise<ContractOutput<TransactionHash>> => unimplemented();

/**
 * Claim the auction lot and deposit if the auction lot has not been purchased
 * by the winning bidder before purchase deadline, distribute the auction fees
 * to the delegates.
 */
export const claimAuctionLotSeller = async (
  auctionCs: CurrencySymbol
): Promise<ContractOutput<TransactionHash>> => unimplemented();

// Auctions (bidder) -----------------------------------------------------------

/**
 * Apply for participation in the auction by sending a deposit equal to or
 * greater than the `minDepositAmount` specified in `AuctionTerms`.
 *
 * If `depositAmount` is set to `null`, the minimum deposit will be made.
 */
export const enterAuction = async (
  walletApp: WalletApp,
  auctionCs: CurrencySymbol,
  bidderVk: VerificationKey,
  depositAmount: BigInt | null
): Promise<ContractOutput<TransactionHash>> => unimplemented();

/**
 * Discover the seller's signature required for placing bids.
 */
export const discoverSellerSignature = async (
  auctionCs: CurrencySymbol,
  bidderVk: VerificationKey
): Promise<ContractOutput<ByteArray>> => unimplemented();

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

// Auctions (anyone) -----------------------------------------------------------

export const queryAuctions = async (
  walletApp: WalletApp | null
): Promise<Array<AuctionInfo>> => Purs.queryAuctions(walletApp)();

export const cleanupAuction = async (
  auctionCs: CurrencySymbol
): Promise<ContractOutput<TransactionHash>> => unimplemented();

// Auctions (delegate) ---------------------------------------------------------

export const announceDelegateGroup = async (
  groupName: string,
  groupUrl: string,
  delegates: Array<PubKeyHash>
): Promise<ContractOutput<TxCbor>> => unimplemented();

// Helpers ---------------------------------------------------------------------

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
