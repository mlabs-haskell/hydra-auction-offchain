.PHONY: requires-nix-shell build bundle bundle-docker serve repl \
			  format check plutip-test plutip-env delegate-server-help \
        delegate-cluster

purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,UserDefinedWarning"
ha-frontend-api := ha-frontend-api

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell, run 'nix develop' first" \
				&& false \
		)

format:
	@nix run .#pursFormat && nix run .#jsFormat && nix run .#nixFormat

check:
	@nix build .#checks.x86_64-linux.all

bundle-docker:
	docker rm -f ${ha-frontend-api}
	docker build \
		--build-arg CARDANO_NETWORK=${CARDANO_NETWORK} \
		--build-arg BLOCKFROST_API_KEY=${BLOCKFROST_API_KEY} \
		-t ${ha-frontend-api} \
		-f docker/frontend-api/Dockerfile \
		.
	docker create --name ${ha-frontend-api} ${ha-frontend-api}
	docker cp ${ha-frontend-api}:/app/dist .
	docker rm -f ${ha-frontend-api}

build: requires-nix-shell
	spago build --purs-args ${purs-args}

bundle: build requires-nix-shell
	node bundle.js && tsc --emitDeclarationOnly

serve: bundle requires-nix-shell
	cd demo && npm run serve

repl: requires-nix-shell
	spago repl

plutip-test: requires-nix-shell
	CARDANO_NETWORK=mainnet spago run --main Test.Plutip

plutip-env: requires-nix-shell
	spago run --main PlutipEnv.Main --exec-args "--payment-skey-file plutip-env/payment.skey" 

delegate-server-help: requires-nix-shell
	spago run --main DelegateServer.Main --exec-args '--help'

delegate-cluster:
	docker compose -f docker/delegate-server/docker-compose.yaml up --build
