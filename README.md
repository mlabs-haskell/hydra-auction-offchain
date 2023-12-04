# hydra-auction-offchain

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Environment](#environment)
- [Workflows](#workflows)
  - [Use as an NPM dependency](#use-as-an-npm-dependency)
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
export NPM_ENV=1
export CARDANO_NETWORK=preprod
export BLOCKFROST_API_KEY=<your key>
export PLUTIP_ENV_HOST_PORT=localhost:8083 
export DEMO_HOST_PORT=localhost:8080
```

**Important**: Set `NPM_ENV` to 1 if you intend to [use `hydra-auction-offchain`
as an NPM dependency](#use-as-an-npm-dependency). If you need to enter the Nix development environment, 
leave this variable unset.

Note: Blockfrost API key for preprod network can be generated at 
[Blockfrost](https://blockfrost.io/).

## Workflows

### Use as an NPM dependency

The easiest way to start using `hydra-auction-offchain` is to specify it as a
git dependency in your `package.json`. Running `npm install` from within your
project will fetch the library from GitHub and generate the `dist` folder using
the environment variables set beforehand.  

**Important**: Before executing `npm install`, ensure that you have set 
`NPM_ENV` environment variable to 1 (don't forget to set other required env
variables too). Otherwise, the necessary `postinstall` and `prepare` npm scripts
won't run, resulting in the failure to generate the `dist` folder.

Specify the dependency in `package.json` as follows:

```json
"dependencies": {
  "hydra-auction-offchain": "git+ssh://git@github.com:mlabs-haskell/hydra-auction-offchain"
}
```

Then, import the API into your project as shown below:

```TypeScript
import {
  announceAuction,
  awaitTxConfirmed,
  mintTokenUsingAlwaysMints,
  queryAuctions
} from "hydra-auction-offchain";
import type {
  AnnounceAuctionContractParams,
  ContractOutput,
  POSIXTime,
  TokenName,
  TransactionHash,
  WalletApp
} from "hydra-auction-offchain";
```

For a complete example, refer to [demo/src/index.ts](./demo/src/index.ts).

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
Plutip wallet: 

```TypeScript
const walletApp: WalletApp = "Plutip";
```

4. In the second shell, run `make serve`.

Note: Prior to announcing the auction, ensure that the auction lot tokens have
been minted and placed in the wallet, otherwise the `AnnounceAuction` contract
will fail with error code `AnnounceAuction04`. For testing purposes, you can
utilize the provided `mintTokenUsingAlwaysMints` function to mint tokens using
the `AlwaysMints` minting policy.

Note: It is recommended to stop the plutip-env service by entering the `stop` 
command to stdin. This ensures the correct deallocation of resources.
