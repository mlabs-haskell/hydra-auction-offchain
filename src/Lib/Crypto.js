/* global BROWSER_RUNTIME */

let csl;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  csl = await import("@emurgo/cardano-serialization-lib-browser");
} else {
  csl = await import("@emurgo/cardano-serialization-lib-nodejs");
}

// verifySignature :: ByteArray -> ByteArray -> ByteArray -> Effect Boolean
export const verifySignature = (vkeyBytes) => (message) => (signatureBytes) => () => {
  const publicKey = csl.PublicKey.from_bytes(vkeyBytes);
  const signature = csl.Ed25519Signature.from_bytes(signatureBytes);
  return publicKey.verify(message, signature);
};
