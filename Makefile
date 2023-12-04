.PHONY: build, bundle, serve, repl, format, check-format, plutip-env

ps-sources := $(shell fd --no-ignore-parent -epurs)
ts-js-sources := $(shell fd --no-ignore-parent -ets -ejs)
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
	@purs-tidy format-in-place ${ps-sources} && prettier -w ${ts-js-sources}

check-format:
	@purs-tidy check ${ps-sources} && prettier -c ${ts-js-sources}

plutip-env:
	spago run --main PlutipEnv.Main --exec-args "--payment-skey-file plutip-env/payment.skey" 
