# hydra-auction-offchain

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Environment](#environment)
- [Workflows](#workflows)
  - [Bundle](#bundle)
  - [Serve demo](#serve-demo)
  - [Deploy locally using Plutip](#deploy-locally-using-plutip)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Environment

Before executing any commands, make sure you are within the Nix development 
environment and have configured the necessary environment variables. 
Follow these steps:

1. Run `nix develop` in the root of the repository to enter the dev environment.

2. Set the required environment variables:

```shell
export CARDANO_NETWORK=preprod
export BLOCKFROST_API_KEY=<your key>
export PLUTIP_ENV_HOST_PORT=localhost:8083 
export DEMO_HOST_PORT=localhost:8080
```

Note: Blockfrost API key for preprod network can be generated at 
[Blockfrost](https://blockfrost.io/).

## Workflows

### Bundle

To bundle the project for the browser, run:

```
make bundle
```

This command will compile the PureScript code, bundle it with the TypeScript API
using `esbuild`, and generate a `dist` folder that is ready for import into your
codebase:

```TypeScript
import { announceAuction, queryAuctions } from "./dist";
import type {
  AnnounceAuctionContractParams,
  ContractOutput,
  TransactionHash,
  WalletApp
} from "./dist";
```

For a more detailed example on how to use the TypeScript API, refer to 
[demo/src/index.ts](./demo/src/index.ts).

### Serve demo

To serve the demo on localhost, run:

```
make serve
```

This command will [bundle](#bundle) the project, spin up a simple HTTP server on localhost
and execute the code in [demo/src/index.ts](./demo/src/index.ts).

### Deploy locally using Plutip

It is also possible to test the contracts against a locally deployed testnet
network using [Plutip](https://github.com/mlabs-haskell/plutip). To run the demo
against a Plutip network, follow these steps:

1. Open 2 shell windows and set up dev environment in both of them.
2. In the first shell, execute `make plutip-env` to spin up a disposable private 
network. This will generate a wallet, pre-fund it with a substantial amount of
ADA, and start an HTTP server to communicate the private key of the generated 
wallet to the frontend code.
3. Update the demo code in [demo/src/index.ts](./demo/src/index.ts) to use the
`Plutip` wallet.
4. In the second shell, run `make serve`.
