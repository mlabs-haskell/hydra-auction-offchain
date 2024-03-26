/* global BROWSER_RUNTIME */

let validator;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  validator = await import("../../scripts/auction_escrow_validator.plutus");
  validator = validator.default;
} else {
  const fs = await import("fs");
  validator = fs.readFileSync("scripts/auction_escrow_validator.plutus", "utf8");
}

export const auctionEscrowValidator = validator;
