.PHONY: build bundle serve repl format check plutip-env

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
