.PHONY: build bundle serve repl format check plutip-test plutip-env delegate-server1 delegate-server2

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
	spago run --main Test.Plutip

plutip-env:
	spago run --main PlutipEnv.Main --exec-args "--payment-skey-file plutip-env/payment.skey" 

delegate-server1:
	spago run --main DelegateServer.Main --exec-args "\
		--auction-metadata-oref 721e982e42e7d918825464964238f36783656d6b7ead22a332cd9bfb456d7a60#0 \
		--server-port :7010 \
		--hydra-node-id A \
		--hydra-node 127.0.0.1:7000 \
		--hydra-node-api 127.0.0.1:7001 \
		--hydra-persist-dir ./hydra-persist \
		--hydra-sk hydra.sk \
		--cardano-sk cardano.sk \
		--wallet-sk wallet.sk \
		--node-socket-preprod ~/state-node-preprod/node.socket \
		--blockfrost-api-key ${BLOCKFROST_API_KEY} \
		--peer '{ \"hydraNode\": \"127.0.0.1:7002\", \"hydraVk\": \"hydra2.vk\", \"cardanoVk\": \"cardano2.vk\" }'"

delegate-server2:
	spago run --main DelegateServer.Main --exec-args "\
		--auction-metadata-oref 721e982e42e7d918825464964238f36783656d6b7ead22a332cd9bfb456d7a60#0  \
		--server-port :7011 \
		--hydra-node-id B \
		--hydra-node 127.0.0.1:7002 \
		--hydra-node-api 127.0.0.1:7003 \
		--hydra-persist-dir ./hydra-persist \
		--hydra-sk hydra2.sk \
		--cardano-sk cardano2.sk \
		--wallet-sk wallet2.sk \
		--node-socket-preprod ~/state-node-preprod/node.socket \
		--blockfrost-api-key ${BLOCKFROST_API_KEY} \
		--peer '{ \"hydraNode\": \"127.0.0.1:7000\", \"hydraVk\": \"hydra.vk\", \"cardanoVk\": \"cardano.vk\" }'"
