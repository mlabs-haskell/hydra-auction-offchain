import * as Purs from "../output/HydraAuctionOffchain.Api/index.js";

import type {
  AnnounceAuctionContractParams,
  AuctionInfo,
  BigInt,
  ContractOutput,
  TokenName,
  TransactionHash,
  WalletApp
} from "./types";

// Auctions --------------------------------------------------------------------

export const announceAuction = async (
  walletApp: WalletApp,
  params: AnnounceAuctionContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.announceAuction(walletApp)(params)();

export const queryAuctions = async (
  walletApp: WalletApp | null
): Promise<Array<AuctionInfo>> => Purs.queryAuctions(walletApp)();

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
