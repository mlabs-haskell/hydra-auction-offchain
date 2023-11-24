import * as Purs from "../output/HydraAuctionOffchain.Api/index.js";

import type {
  AnnounceAuctionContractParams,
  AuctionInfo,
  ContractOutput,
  TransactionHash,
  WalletApp
} from "./types";

export const announceAuction = async (
  walletApp: WalletApp,
  params: AnnounceAuctionContractParams
): Promise<ContractOutput<TransactionHash>> => Purs.announceAuction(walletApp)(params)();

export const queryAuctions = async (): Promise<Array<AuctionInfo>> => Purs.queryAuctions();
