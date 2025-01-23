import JSONbig from "@mlabs-haskell/json-bigint";

export const stringifyAesonWithIndent = (indent) => (aeson) => {
  return JSONbig.stringify(aeson, null, indent);
};
