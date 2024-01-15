import {
  announceAuction,
  authorizeBidders,
  awaitTxConfirmed,
  discoverBidders,
  discoverSellerSignature,
  enterAuction,
  mintTokenUsingAlwaysMints,
  queryAuctions,
  queryStandingBidState,
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
  const walletApp: WalletApp = "Plutip";
  const biddingStart = walletApp === "Plutip" ? 5000 : 90000;

  const tokenName: TokenName = "4d6f6e614c697361"; // MonaLisa
  const mintTxHash = await mintTokenUsingAlwaysMints(walletApp, tokenName, "1");
  await awaitTxConfirmed(walletApp, mintTxHash);

  // seller: announceAuction
  const announceAuctionResult = await runAnnounceAuction(walletApp, tokenName, biddingStart);
  console.log("AnnounceAuction:", announceAuctionResult);
  if (announceAuctionResult.tag !== "result") return;
  await awaitTxConfirmed(walletApp, announceAuctionResult.value.txHash);
  const auctionInfo = announceAuctionResult.value.auctionInfo;

  // bidder: queryAuctions
  const auctions = await queryAuctions(walletApp);
  console.log("Auctions:", auctions);

  // bidder: enterAuction (stub)
  const depositAmount = "3000000";
  const enterAuctionResult = await enterAuction(walletApp, { auctionInfo, depositAmount });
  console.log("EnterAuction:", enterAuctionResult);

  // seller: discoverBidders (stub)
  const bidders = await discoverBidders(walletApp, auctionInfo);
  console.log("Candidate bidders:", bidders);

  // seller: authorizeBidders (stub)
  const auctionCs = auctionInfo.auctionId;
  const biddersToAuthorize = bidders.map((bidder) => bidder.bidderVk);
  const authBiddersParams = { auctionCs, biddersToAuthorize };
  const authBiddersResult = await authorizeBidders(walletApp, authBiddersParams);
  console.log("AuthorizeBidders:", authBiddersResult);

  // bidder: discoverSellerSignature (stub)
  const sellerPkh = auctionInfo.auctionTerms.sellerPkh;
  let sellerSignature: ByteArray | null = null;
  while (sellerSignature === null) {
    sellerSignature = await discoverSellerSignature(walletApp, { auctionCs, sellerPkh });
  }
  console.log("Seller signature:", sellerSignature);

  // seller: startBidding
  await delay(biddingStart + 2000);
  const startBiddingResult = await startBidding(walletApp, { auctionInfo });
  console.log("StartBidding:", startBiddingResult);
  if (startBiddingResult.tag !== "result") return;
  await awaitTxConfirmed(walletApp, startBiddingResult.value);

  // bidder: queryStandingBidState (stub)
  const bidState = await queryStandingBidState(walletApp, auctionInfo);
  console.log("Standing bid:", bidState);
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
