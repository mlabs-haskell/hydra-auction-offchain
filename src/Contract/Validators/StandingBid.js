/* global BROWSER_RUNTIME */

let validator;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  validator = await import("../../scripts/standing_bid_validator.plutus");
  validator = validator.default;
} else {
  const fs = await import("fs");
  validator = fs.readFileSync("scripts/standing_bid_validator.plutus", "utf8");
}

export const standingBidValidator = validator;
