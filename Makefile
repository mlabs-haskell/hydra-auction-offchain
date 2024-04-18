.PHONY: build bundle serve repl format check plutip-test plutip-env delegate-server-help delegate-cluster

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

delegate-cluster:
	docker compose -f docker/delegate-server/docker-compose.yaml up --build
