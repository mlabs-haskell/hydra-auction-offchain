/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = await import("@emurgo/cardano-message-signing-browser");
} else {
  lib = await import("@emurgo/cardano-message-signing-nodejs");
}

// fromBytesCoseKey :: CborBytes -> Effect CoseKey
export const fromBytesCoseKey = (bytes) => () => {
  return lib.COSEKey.from_bytes(bytes);
};

// getCoseKeyHeaderX :: MaybeFfiHelper -> CoseKey -> Maybe ByteArray
export const getCoseKeyHeaderX = (maybe) => (coseKey) => {
  const cborValue = coseKey.header(
    lib.Label.new_int(
      lib.Int.new_negative(lib.BigNum.from_str("2")) // x (-2)
    )
  );
  return opt_chain(maybe, cborValue, "as_bytes");
};

// Helpers

function opt_chain(maybe, obj) {
  const isNothing = (x) => x === null || x === undefined;
  let result = obj;
  for (let i = 2; i < arguments.length; i++) {
    if (isNothing(result)) {
      return maybe.nothing;
    } else {
      result = result[arguments[i]]();
    }
  }
  return isNothing(result) ? maybe.nothing : maybe.just(result);
}
