.PHONY: requires-nix-shell build bundle bundle-docker serve repl \
			  format check-format localnet-env delegate-server-help \
        delegate-cluster delegate-cluster-cleanup test test-nix \
				build-delegate-server-nix

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
js-sources := $(shell fd --no-ignore-parent -ejs)
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport,UserDefinedWarning"
ha-frontend-api := ha-frontend-api
delegate-cluster-docker-compose := docker/delegate-cluster/docker-compose.yaml

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell, run 'nix develop' first." \
				&& false \
		)

build: requires-nix-shell
	spago build --purs-args ${purs-args}

test: requires-nix-shell
	CARDANO_NETWORK=mainnet spago run --main Test.Main

test-nix:
	nix run .#checks.x86_64-linux.hydra-auction-offchain-tests

format: requires-nix-shell
	@purs-tidy format-in-place ${ps-sources}
	@nixpkgs-fmt ${nix-sources}
	@prettier -w ${js-sources}

check-format:
	nix run .#checks.x86_64-linux.formatting-check

bundle: build requires-nix-shell
	node bundle.js && tsc --emitDeclarationOnly

bundle-docker:
	docker rm -f ${ha-frontend-api}
	docker build -t ${ha-frontend-api} -f docker/frontend-api/Dockerfile .
	docker create --name ${ha-frontend-api} ${ha-frontend-api}
	docker cp ${ha-frontend-api}:/app/dist .
	docker rm -f ${ha-frontend-api}

serve:
	cd demo && npm install --package-lock-only=false && npm run serve

repl: requires-nix-shell
	spago repl

localnet-env: requires-nix-shell
	spago run --main PlutipEnv.Main --exec-args "--payment-skey-file plutip-env/payment.skey" 

build-delegate-server-nix:
	nix build .#packages.x86_64-linux.delegate-server

delegate-server-help: requires-nix-shell
	spago run --main DelegateServer.Main --exec-args '--help'

delegate-cluster:
	docker compose -f ${delegate-cluster-docker-compose} up --build --no-attach cardano-node

delegate-cluster-cleanup:
	docker compose -f ${delegate-cluster-docker-compose} rm --force --stop --volumes
