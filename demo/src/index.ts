import {
  announceAuction,
  authorizeBidders,
  awaitTxConfirmed,
  claimAuctionLotBidder,
  claimAuctionLotSeller,
  claimDepositLoser,
  cleanupAuction,
  discoverBidders,
  discoverSellerSignature,
  enterAuction,
  mintTokenUsingAlwaysMints,
  placeBid,
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

async function logConfirmContract<T extends { txHash: TransactionHash }>(
  label: string,
  walletApp: WalletApp,
  output: ContractOutput<T | TransactionHash>
): Promise<void> {
  console.log(label + ":", output);
  if (output.tag !== "result") {
    throw new Error(label + " contract failed.");
  }
  await awaitTxConfirmed(walletApp, output.value.txHash ?? output.value);
}

(async () => {
  await delay(1000); // need some time for cardano object to be injected
  const walletApp: WalletApp = "Nami";
  const preBiddingPeriod = walletApp === "Plutip" ? 15000 : 90000;
  const biddingPeriod = preBiddingPeriod;

  const tokenName: TokenName = "4d6f6e614c697361"; // MonaLisa
  const mintTxHash = await mintTokenUsingAlwaysMints(walletApp, tokenName, "1");
  await awaitTxConfirmed(walletApp, mintTxHash);

  // seller: announceAuction
  const announceAuctionResult = await runAnnounceAuction(
    walletApp,
    tokenName,
    preBiddingPeriod,
    biddingPeriod
  );
  await logConfirmContract("AnnounceAuction", walletApp, announceAuctionResult);
  const auctionInfo = announceAuctionResult.value.auctionInfo;

  // seller: queryAuctions
  const sellerAuctions = await queryAuctions(walletApp, { myRole: "ActorRoleSeller" });
  console.log("Seller auctions:", sellerAuctions);

  // bidder: queryAuctions
  const auctions = await queryAuctions(walletApp);
  console.log("All auctions:", auctions);

  // bidder: enterAuction
  const depositAmount = "2800000";
  const enterAuctionResult = await enterAuction(walletApp, { auctionInfo, depositAmount });
  await logConfirmContract("EnterAuction", walletApp, enterAuctionResult);

  // bidder: queryAuctions
  const bidderAuctions = await queryAuctions(walletApp, { myRole: "ActorRoleBidder" });
  console.log("Bidder auctions:", bidderAuctions);

  // seller: discoverBidders
  const bidders = await discoverBidders(walletApp, auctionInfo);
  console.log("Candidate bidders:", bidders);

  // seller: authorizeBidders
  const auctionCs = auctionInfo.auctionId;
  const biddersToAuthorize = bidders.map((bidder) => bidder.bidderInfo.bidderVk);
  const authBiddersParams = { auctionCs, biddersToAuthorize };
  const authBiddersResult = await authorizeBidders(walletApp, authBiddersParams);
  await logConfirmContract("AuthorizeBidders", walletApp, authBiddersResult);

  // bidder: discoverSellerSignature
  const sellerAddress = auctionInfo.auctionTerms.sellerAddress;
  const sellerSignature = await discoverSellerSignature(walletApp, {
    auctionCs,
    sellerAddress
  });
  console.log("Seller signature:", sellerSignature);

  // seller: startBidding
  await delay(preBiddingPeriod + 2000);
  const startBiddingResult = await startBidding(walletApp, { auctionInfo });
  await logConfirmContract("StartBidding", walletApp, startBiddingResult);

  // bidder: placeBid
  if (sellerSignature.tag === "result") {
    const placeBidParams = {
      auctionInfo,
      sellerSignature: sellerSignature.value,
      bidAmount: "10000000"
    };
    const placeBidResult = await placeBid(walletApp, placeBidParams);
    await logConfirmContract("PlaceBid", walletApp, placeBidResult);
  }

  // bidder: queryStandingBidState
  const bidState = await queryStandingBidState(walletApp, auctionInfo);
  console.log("Standing bid:", bidState);

  // bidder: claimAuctionLotBidder
  await delay(biddingPeriod + 2000);
  const claimAuctionLotBidderResult = await claimAuctionLotBidder(walletApp, auctionInfo);
  await logConfirmContract("ClaimAuctionLotBidder", walletApp, claimAuctionLotBidderResult);

  // bidder: claimDepositLoser (stub)
  const claimDepositLoserResult = await claimDepositLoser(walletApp, auctionInfo);
  console.log("ClaimDepositLoser:", claimDepositLoserResult);

  // seller: claimAuctionLotSeller (stub)
  const claimAuctionLotSellerResult = await claimAuctionLotSeller(walletApp, auctionInfo);
  console.log("ClaimAuctionLotSeller:", claimAuctionLotSellerResult);

  // anyone: cleanupAuction (stub)
  const cleanupAuctionResult = await cleanupAuction(walletApp, auctionInfo);
  console.log("CleanupAuction:", cleanupAuctionResult);
})();

async function runAnnounceAuction(
  walletApp: WalletApp,
  tokenName: TokenName,
  preBiddingPeriod: number,
  biddingPeriod: number
): Promise<ContractOutput<TransactionHash>> {
  const nowTime = Date.now();
  const biddingStart = nowTime + preBiddingPeriod;
  const biddingEnd = biddingStart + biddingPeriod;
  const purchaseDeadline = biddingEnd + 60000;
  const cleanup = purchaseDeadline + 60000;
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
      biddingStart: biddingStart.toString(),
      biddingEnd: biddingEnd.toString(),
      purchaseDeadline: purchaseDeadline.toString(),
      cleanup: cleanup.toString(),
      auctionFeePerDelegate: "5000000",
      startingBid: "6000000",
      minBidIncrement: "1000000",
      minDepositAmount: "3000000"
    },
    additionalAuctionLotOrefs: []
  };
  return await announceAuction(walletApp, params);
}
