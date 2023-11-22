import * as Purs from "../output/HydraAuctionOffchain.Api/index.js";

export const announceAuction = async (walletApp: any, params: any): Promise<any> =>
  Purs.announceAuction(walletApp)(params)();
