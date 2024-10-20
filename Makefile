.PHONY: requires-nix-shell build bundle bundle-docker serve repl \
			  format check plutip-test plutip-env delegate-server-help \
        delegate-cluster delegate-cluster-cleanup

purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,UserDefinedWarning"
ha-frontend-api := ha-frontend-api
delegate-cluster-docker-compose := docker/delegate-cluster/docker-compose.yaml

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell, run 'nix develop' first." \
				&& false \
		)

format:
	@nix run .#pursFormat && nix run .#jsFormat && nix run .#nixFormat

check:
	@nix build .#checks.x86_64-linux.all

build: requires-nix-shell
	spago build --purs-args ${purs-args}

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

plutip-test: requires-nix-shell
	CARDANO_NETWORK=mainnet spago run --main Test.Plutip

plutip-env: requires-nix-shell
	spago run --main PlutipEnv.Main --exec-args "--payment-skey-file plutip-env/payment.skey" 

delegate-server-help: requires-nix-shell
	spago run --main DelegateServer.Main --exec-args '--help'

delegate-cluster:
	docker compose -f ${delegate-cluster-docker-compose} up --build

delegate-cluster-cleanup:
	docker compose -f ${delegate-cluster-docker-compose} rm --force --stop --volumes
