/* global BROWSER_RUNTIME */

let validator;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  validator = await import("../../scripts/delegate_group_metadata_validator.plutus");
  validator = validator.default;
} else {
  const fs = await import("fs");
  validator = fs.readFileSync("scripts/delegate_group_metadata_validator.plutus", "utf8");
}

export const delegateGroupMetadataValidator = validator;
