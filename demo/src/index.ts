import { announceAuction, queryAuctions } from "../../dist";
import type {
  AnnounceAuctionContractParams,
  ContractOutput,
  TransactionHash,
  WalletApp
} from "../../dist";

(async () => {
  await delay(1000); // need some time for cardano object to be injected
  const result = await runAnnounceAuction();
  console.log("AnnounceAuction:", result);

  const auctions = await queryAuctions();
  console.log("QueryAuctions:", auctions);
})();

function delay(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function runAnnounceAuction(): Promise<ContractOutput<TransactionHash>> {
  const walletApp: WalletApp = "Nami";
  const params: AnnounceAuctionContractParams = {
    auctionTerms: {
      auctionLot: [
        {
          cs: "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d",
          tn: "4d6f6e614c697361",
          quantity: "1"
        }
      ],
      sellerPkh: "2bcc4ca387f39d2e792d7d08484c96d0d59b26cbfafc1fa4ffad486c",
      sellerVk: "400d8729d8be39372f3bc6241af2f0e44892a4e065848d39c1006d2329e64db9",
      delegates: ["2bcc4ca387f39d2e792d7d08484c96d0d59b26cbfafc1fa4ffad486c"],
      biddingStart: "1764241203000",
      biddingEnd: "1764327603000",
      purchaseDeadline: "1764586803000",
      cleanup: "1764673203000",
      auctionFeePerDelegate: "5000000",
      startingBid: "6000000",
      minBidIncrement: "1000000",
      minDepositAmount: "3000000"
    },
    additionalAuctionLotOrefs: []
  };
  return await announceAuction(walletApp, params);
}
