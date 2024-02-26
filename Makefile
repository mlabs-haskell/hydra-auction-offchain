.PHONY: build bundle serve repl format check plutip-env delegate-server

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

plutip-env:
	spago run --main PlutipEnv.Main --exec-args "--payment-skey-file plutip-env/payment.skey" 

delegate-server:
	spago run --main DelegateServer.Main --exec-args "\
		--hydra-node 127.0.0.1:7000 \
		--hydra-node-api 127.0.0.1:7001 \
		--hydra-sk hydra.sk \
		--cardano-sk cardano.sk \
		--node-socket-preprod ~/state-node-preprod/node.socket"
