.PHONY: build, format, check-format

ps-sources := $(shell fd --no-ignore-parent -epurs)
js-sources := $(shell fd --no-ignore-parent -ejs)
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,UserDefinedWarning"

build:
	spago build --purs-args ${purs-args}

format:
	@purs-tidy format-in-place ${ps-sources} && prettier --config .prettierrc -w ${js-sources}

check-format:
	@purs-tidy check ${ps-sources}
