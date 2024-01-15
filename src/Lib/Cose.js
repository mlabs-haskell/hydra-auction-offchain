/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-message-signing-browser");
} else {
  lib = await import("@emurgo/cardano-message-signing-nodejs");
}

// -------------------------------------------------------------------
// COSESign1

export const getCoseSign1Signature = (coseSign1Bytes) => () =>
  lib.COSESign1.from_bytes(coseSign1Bytes).signature();

// -------------------------------------------------------------------
// SigStructure

// newSigStructure :: ByteArray -> ProtectedHeaderMap -> ByteArray
export const newSigStructure = (payload) => (headers) =>
  lib.SigStructure.new(lib.SigContext.Signature1, headers, [], payload).to_bytes();

// -------------------------------------------------------------------
// ProtectedHeaderMap

// newProtectedHeaderMap :: HeaderMap -> ProtectedHeaderMap
export const newProtectedHeaderMap = (headerMap) => lib.ProtectedHeaderMap.new(headerMap);

// -------------------------------------------------------------------
// HeaderMap

// newHeaderMap :: Effect HeaderMap
export const newHeaderMap = () => lib.HeaderMap.new();

// setAlgHeaderToEdDsa :: HeaderMap -> Effect Unit
export const setAlgHeaderToEdDsa = (headerMap) => () => {
  const label = lib.Label.from_algorithm_id(lib.AlgorithmId.EdDSA);
  headerMap.set_algorithm_id(label);
};

// setAddressHeader :: ByteArray -> HeaderMap -> Effect Unit
export const setAddressHeader = (addressBytes) => (headerMap) => () => {
  const label = lib.Label.new_text("address");
  const value = lib.CBORValue.new_bytes(addressBytes);
  headerMap.set_header(label, value);
};
