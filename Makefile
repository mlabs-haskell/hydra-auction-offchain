.PHONY: build, format, check-format

ps-sources := $(shell fd --no-ignore-parent -epurs)
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,UserDefinedWarning"

build:
	spago build --purs-args ${purs-args}

format:
	@purs-tidy format-in-place ${ps-sources}

check-format:
	@purs-tidy check ${ps-sources}
