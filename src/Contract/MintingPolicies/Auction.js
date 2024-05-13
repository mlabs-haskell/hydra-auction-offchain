/* global BROWSER_RUNTIME */

let mintingPolicy;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  mintingPolicy = await import("../../scripts/auction_minting_policy.plutus");
  mintingPolicy = mintingPolicy.default;
} else {
  const fs = await import("fs");
  mintingPolicy = fs.readFileSync("scripts/auction_minting_policy.plutus", "utf8");
}

export const auctionMintingPolicy = mintingPolicy;
