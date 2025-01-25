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
  getWalletVk,
  mintTokenUsingAlwaysMints,
  moveBidL2,
  placeBidL2,
  queryAuctions,
  queryDelegateGroups,
  queryStandingBidState,
  registerDelegateGroup,
  startBidding
} from "hydra-auction-offchain";
import type {
  AnnounceAuctionContractParams,
  ContractConfig,
  ContractOutput,
  POSIXTime,
  TokenName,
  TransactionHash
} from "hydra-auction-offchain";

function delay(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function logConfirmContract<T extends { txHash: TransactionHash }>(
  label: string,
  config: ContractConfig,
  output: ContractOutput<T | TransactionHash>
): Promise<void> {
  console.log(label + ":", output);
  if (output.tag !== "result") {
    throw new Error(label + " contract failed.");
  }
  await awaitTxConfirmed(config, output.value.txHash ?? output.value);
}

(async () => {
  await delay(1000); // need some time for cardano object to be injected
  const config: ContractConfig = {
    tag: "network",
    network: "Preprod",
    blockfrostApiKey: process.env.BLOCKFROST_API_KEY,
    walletApp: "Nami"
  };
  const preBiddingPeriod = config.tag === "plutip" ? 15000 : 90000;
  const biddingPeriod = 600000; // 10 min

  const tokenName: TokenName = "4d6f6e614c697361"; // MonaLisa
  const mintTxHash = await mintTokenUsingAlwaysMints(config, tokenName, "1");
  await awaitTxConfirmed(config, mintTxHash);

  // delegate group: registerDelegateGroup
  const registerDelegateGroupResult = await registerDelegateGroup(
    config,
    { delegateGroupServers: {
        httpServers: ["http://127.0.0.1:7010", "http://127.0.0.1:7011"],
        wsServers: ["ws://127.0.0.1:7020", "ws://127.0.0.1:7021"]
      },
      delegateGroupMetadata: "Test delegate group"
    }
  );
  await logConfirmContract("RegisterDelegateGroup", config, registerDelegateGroupResult);

  // seller: queryDelegateGroups
  const delegateGroups = await queryDelegateGroups(config);
  console.log("Delegate groups:", delegateGroups);

  const delegateInfo = delegateGroups[0]?.delegateGroupServers;
  if (!delegateInfo) throw new Error("No registered delegate groups found.");

  // seller: announceAuction
  const announceAuctionResult = await runAnnounceAuction(
    config,
    delegateInfo,
    tokenName,
    preBiddingPeriod,
    biddingPeriod
  );
  await logConfirmContract("AnnounceAuction", config, announceAuctionResult);
  const auctionInfo = announceAuctionResult.value.auctionInfo;
  const auctionCs = auctionInfo.auctionId;
  const auctionTerms = auctionInfo.auctionTerms;

  // seller: queryAuctions
  const sellerAuctions = await queryAuctions(config, { myRole: "ActorRoleSeller" });
  console.log("Seller auctions:", sellerAuctions);

  // bidder: queryAuctions
  const auctions = await queryAuctions(config);
  console.log("All auctions:", auctions);

  // bidder: enterAuction
  const depositAmount = "2800000";
  const enterAuctionResult = await enterAuction(config, { auctionInfo, depositAmount });
  await logConfirmContract("EnterAuction", config, enterAuctionResult);

  // bidder: queryAuctions
  const bidderAuctions = await queryAuctions(config, { myRole: "ActorRoleBidder" });
  console.log("Bidder auctions:", bidderAuctions);

  // seller: discoverBidders
  const bidders = await discoverBidders(config, auctionInfo);
  console.log("Candidate bidders:", bidders);

  // seller: authorizeBidders
  const biddersToAuthorize = bidders.map((bidder) => bidder.bidderInfo.bidderVk);
  const authBiddersParams = { auctionCs, biddersToAuthorize };
  const authBiddersResult = await authorizeBidders(config, authBiddersParams);
  await logConfirmContract("AuthorizeBidders", config, authBiddersResult);

  // bidder: discoverSellerSignature
  const sellerAddress = auctionInfo.auctionTerms.sellerAddress;
  const bidderVk = await getWalletVk(config);
  const sellerSignature = await discoverSellerSignature(config, {
    auctionCs,
    sellerAddress,
    bidderVk: bidderVk.value
  });
  console.log("Seller signature:", sellerSignature);

  // seller: startBidding
  document.getElementById("startBidding")?.addEventListener("click", async (event) => {
    const startBiddingResult = await startBidding(config, { auctionInfo });
    await logConfirmContract("StartBidding", config, startBiddingResult);
  });

  document.getElementById("moveBidL2")?.addEventListener("click", async (event) => {
    // anyone: moveBidL2
    const moveBidParams = { auctionCs, auctionTerms, delegateInfo };
    const moveBidResult = await moveBidL2(config, moveBidParams);
    console.log("MoveBid:", moveBidResult);
  });

  document.getElementById("placeBidL2")?.addEventListener("click", async (event) => {
    // bidder: placeBidL2
    const placeBidParams = {
      auctionCs,
      auctionTerms,
      delegateInfo,
      sellerSignature: sellerSignature.value,
      bidAmount: document.getElementById("bidLovelace")?.value
    };
    const placeBidResult = await placeBidL2(config, placeBidParams);
    console.log("PlaceBid:", placeBidResult);
  });

  document.getElementById("claimAuctionLot")?.addEventListener("click", async (event) => {
    // bidder: queryStandingBidState
    const bidState = await queryStandingBidState(config, auctionInfo);
    console.log("Standing bid:", bidState);

    // bidder: claimAuctionLotBidder
    const claimAuctionLotBidderResult = await claimAuctionLotBidder(config, auctionInfo);
    await logConfirmContract("ClaimAuctionLotBidder", config, claimAuctionLotBidderResult);
  });

  /*
  // bidder: queryStandingBidState
  const bidState = await queryStandingBidState(config, auctionInfo);
  console.log("Standing bid:", bidState);

  // bidder: claimAuctionLotBidder
  await delay(biddingPeriod + 2000);
  const claimAuctionLotBidderResult = await claimAuctionLotBidder(config, auctionInfo);
  await logConfirmContract("ClaimAuctionLotBidder", config, claimAuctionLotBidderResult);

  // bidder: claimDepositLoser (stub)
  const claimDepositLoserResult = await claimDepositLoser(config, auctionInfo);
  console.log("ClaimDepositLoser:", claimDepositLoserResult);

  // seller: claimAuctionLotSeller (stub)
  const claimAuctionLotSellerResult = await claimAuctionLotSeller(config, auctionInfo);
  console.log("ClaimAuctionLotSeller:", claimAuctionLotSellerResult);

  // anyone: cleanupAuction (stub)
  const cleanupAuctionResult = await cleanupAuction(config, auctionInfo);
  console.log("CleanupAuction:", cleanupAuctionResult);
  */
})();

async function runAnnounceAuction(
  config: ContractConfig,
  delegateInfo: DelegateInfo,
  tokenName: TokenName,
  preBiddingPeriod: number,
  biddingPeriod: number
): Promise<ContractOutput<TransactionHash>> {
  const nowTime = Date.now();
  const biddingStart = nowTime + preBiddingPeriod;
  const biddingEnd = biddingStart + biddingPeriod + 3600000;
  const purchaseDeadline = biddingEnd + 3600000;
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
      delegates: [],
      biddingStart: biddingStart.toString(),
      biddingEnd: biddingEnd.toString(),
      purchaseDeadline: purchaseDeadline.toString(),
      cleanup: cleanup.toString(),
      auctionFeePerDelegate: "3000000",
      startingBid: "8000000",
      minBidIncrement: "1000000",
      minDepositAmount: "3000000"
    },
    delegateInfo,
    additionalAuctionLotOrefs: []
  };
  return await announceAuction(config, params);
}
