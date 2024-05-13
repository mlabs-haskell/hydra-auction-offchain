export const _isMainnet = process.env.CARDANO_NETWORK === "mainnet";
export const _blockfrostApiKey = process.env.BLOCKFROST_API_KEY ?? "";
export const _plutipEnvHostPort = process.env.PLUTIP_ENV_HOST_PORT ?? "localhost:8083";
export const _demoHostPort = process.env.DEMO_HOST_PORT ?? "localhost:8080";
