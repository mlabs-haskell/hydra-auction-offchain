# Setting up a cluster of Delegate Servers

This document outlines the steps required to spin up a cluster of delegate
servers using Docker Compose.

Within the [docker/delegate-cluster](docker/delegate-cluster)
directory, you can find `docker-compose.yaml` along with JSON configuration 
templates for two delegate servers. 

## Minimal setup

1. Ensure you have a fully-synchronized running Cardano node for the correct
network (preview, preprod, or mainnet).  Follow the instructions provided
[here](https://github.com/IntersectMBO/cardano-node) to install `cardano-node`
and `cardano-cli`.

2. You will also need a `hydra-node` executable to generate Hydra keys later on.
Refer to the `hydra-node` [installation guide](https://hydra.family/head-protocol/docs/getting-started/installation).

2. For 2 delegate servers, generate 6 pairs of keys:
   - `(cardano.vk, cardano.sk)` and `(cardano2.vk, cardano2.sk)`: 
   Cardano keys used by the underlying Hydra nodes.
   ```
   cardano-cli address key-gen \
     --verification-key-file cardano.vk \
     --signing-key-file cardano.sk
   cardano-cli address key-gen \
     --verification-key-file cardano2.vk \
     --signing-key-file cardano2.sk
   ```

   - `(wallet.vk, wallet.sk)` and `(wallet2.vk, wallet2.sk)`: 
   Additional Cardano keys needed to commit collateral to the Hydra Head.
   These keys may become obsolete in the future, but they are necessary because
   `hydra-node` prohibits committing utxos controlled by its Cardano keys.
   ```
   cardano-cli address key-gen \
     --verification-key-file wallet.vk \
     --signing-key-file wallet.sk
   cardano-cli address key-gen \
     --verification-key-file wallet2.vk \
     --signing-key-file wallet2.sk
   ```

   - `(hydra.vk, hydra.sk)` and `(hydra2.vk, hydra2.sk)`: 
   Hydra keys used for multi-signing snapshots within a Hydra Head.
   ```
   hydra-node gen-hydra-key --output-file hydra
   hydra-node gen-hydra-key --output-file hydra2
   ```

3. Place all generated keys in the [keys](keys) directory.

4. Pre-fund addresses associated with the generated Cardano keys with enough ADA.
   - Use `cardano-cli address build` to build addresses.
   - Request test ADA from the [Testnets faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/).

5. Adapt provided JSON configuration templates for delegate servers:
   - `auctionMetadataOref`: Set this field to the auction metadata output
   reference of the auction to be hosted on L2. This data can be extracted from
   auction info after the auction announcement [(see metadataOref field of AuctionInfo)](api/types.ts).
   - Adjust `network` and `queryBackend` fields.
   - `hydraScriptsTxHash` must be set to the hash of the transaction with
   published Hydra scripts in its outputs. See [hydra-node release notes](https://github.com/input-output-hk/hydra/releases)
   for pre-published versions. Make sure to pick the right transaction hash for
   the `hydra-node` version used in the [Dockerfile for delegate-server](docker/delegate-server/Dockerfile)
   and your network (preview, preprod, or mainnet).
   - For this basic setup, all other options **must not be changed**.

6. Set `CARDANO_NODE_SOCKET_PATH` environment variable to filepath to local UNIX
domain socket of your running cardano-node instance.

7. Run `make delegate-cluster` from the root of the repository
(no nix shell required). This command will start delegate-server containers.

8. State of the Hydra nodes will be preserved between calls in Docker volumes.
To remove these volumes, run `make delegate-cluster-cleanup`. You should perform 
the cleanup each time you start a cluster for a new auction.
