.PHONY: build bundle serve repl format check plutip-test plutip-env delegate-server-help delegate-server1 delegate-server2

purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,UserDefinedWarning"

build:
	spago build --purs-args ${purs-args}

bundle: build
	node bundle.js && tsc --emitDeclarationOnly

serve: bundle
	cd demo && npm run serve

repl:
	spago repl

format:
	@nix run .#pursFormat && nix run .#jsFormat && nix run .#nixFormat

check:
	@nix build .#checks.x86_64-linux.all

plutip-test:
	CARDANO_NETWORK=mainnet spago run --main Test.Plutip

plutip-env:
	spago run --main PlutipEnv.Main --exec-args "--payment-skey-file plutip-env/payment.skey" 

delegate-server-help:
	spago run --main DelegateServer.Main --exec-args '--help'

delegate-server1:
	spago run --main DelegateServer.Main --exec-args "\
		--auction-metadata-oref 27f76e09379706939605f2b60ea17b47a881c45fa4681878e9147f08603b437f#0 \
		--server-port :7010 \
		--ws-server-port :7020 \
		--hydra-node-id A \
		--hydra-node 127.0.0.1:7000 \
		--hydra-node-api 127.0.0.1:7001 \
		--hydra-persist-dir ./hydra-persist \
		--hydra-sk hydra.sk \
		--cardano-sk cardano.sk \
		--wallet-sk wallet.sk \
		--node-socket ~/state-node-preprod/node.socket \
		--testnet-magic 1 \
		--blockfrost-config https://cardano-preprod.blockfrost.io:443/api/v0 \
		--blockfrost-api-key ${BLOCKFROST_API_KEY} \
		--hydra-scripts-tx-id 8ce483e2d4b81f9254392afda1f85d1e123165665593228c39064691903f431a \
		--peer '{ \"hydraNode\": \"127.0.0.1:7002\", \"hydraVk\": \"hydra2.vk\", \"cardanoVk\": \"cardano2.vk\" }'"

delegate-server2:
	spago run --main DelegateServer.Main --exec-args "\
		--auction-metadata-oref 27f76e09379706939605f2b60ea17b47a881c45fa4681878e9147f08603b437f#0  \
		--server-port :7011 \
		--ws-server-port :7021 \
		--hydra-node-id B \
		--hydra-node 127.0.0.1:7002 \
		--hydra-node-api 127.0.0.1:7003 \
		--hydra-persist-dir ./hydra-persist \
		--hydra-sk hydra2.sk \
		--cardano-sk cardano2.sk \
		--wallet-sk wallet2.sk \
		--node-socket ~/state-node-preprod/node.socket \
		--testnet-magic 1 \
		--blockfrost-config https://cardano-preprod.blockfrost.io:443/api/v0 \
		--blockfrost-api-key ${BLOCKFROST_API_KEY} \
		--hydra-scripts-tx-id 8ce483e2d4b81f9254392afda1f85d1e123165665593228c39064691903f431a \
		--peer '{ \"hydraNode\": \"127.0.0.1:7000\", \"hydraVk\": \"hydra.vk\", \"cardanoVk\": \"cardano.vk\" }'"
