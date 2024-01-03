import {
  announceAuction,
  awaitTxConfirmed,
  mintTokenUsingAlwaysMints,
  queryAuctions,
  startBidding
} from "hydra-auction-offchain";
import type {
  AnnounceAuctionContractParams,
  ContractOutput,
  POSIXTime,
  TokenName,
  TransactionHash,
  WalletApp
} from "hydra-auction-offchain";

function delay(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

(async () => {
  await delay(1000); // need some time for cardano object to be injected
  const walletApp: WalletApp = "Nami";
  const biddingStart = walletApp === "Plutip" ? 5000 : 90000;

  const tokenName: TokenName = "4d6f6e614c697361"; // MonaLisa
  const mintTxHash = await mintTokenUsingAlwaysMints(walletApp, tokenName, "1");
  await awaitTxConfirmed(walletApp, mintTxHash);

  const announceAuctionResult = await runAnnounceAuction(walletApp, tokenName, biddingStart);
  console.log("AnnounceAuction:", announceAuctionResult);

  if (announceAuctionResult.tag === "result") {
    await awaitTxConfirmed(walletApp, announceAuctionResult.value);
  }

  const auctions = await queryAuctions(walletApp);
  console.log("QueryAuctions:", auctions);

  if (auctions.length > 0) {
    await delay(biddingStart + 2000);

    const auctionInfo = auctions.reduce((acc, x) =>
      BigInt(x.auctionTerms.biddingStart) > BigInt(acc.auctionTerms.biddingStart) ? x : acc
    );

    const startBiddingResult = await startBidding(walletApp, { auctionInfo });
    console.log("StartBidding:", startBiddingResult);

    if (startBiddingResult.tag === "result") {
      await awaitTxConfirmed(walletApp, startBiddingResult.value);
    }
  }
})();

async function runAnnounceAuction(
  walletApp: WalletApp,
  tokenName: TokenName,
  biddingStart: number
): Promise<ContractOutput<TransactionHash>> {
  const nowTime = Date.now();
  const params: AnnounceAuctionContractParams = {
    auctionTerms: {
      auctionLot: [
        {
          cs: "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d", // AlwaysMints
          tn: tokenName,
          quantity: "1"
        }
      ],
      delegates: ["2bcc4ca387f39d2e792d7d08484c96d0d59b26cbfafc1fa4ffad486c"],
      biddingStart: (nowTime + biddingStart).toString(),
      biddingEnd: (nowTime + 600000).toString(),
      purchaseDeadline: (nowTime + 600000 * 2).toString(),
      cleanup: (nowTime + 600000 * 3).toString(),
      auctionFeePerDelegate: "5000000",
      startingBid: "6000000",
      minBidIncrement: "1000000",
      minDepositAmount: "3000000"
    },
    additionalAuctionLotOrefs: []
  };
  return await announceAuction(walletApp, params);
}
