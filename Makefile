j ?= -j 6

ifndef NoFast
fast = --fast
endif

ifdef build_args
build_ghc_opts = --ghc-options  $(build_args)
endif

HASKELL_SOURCES := ./src ./app site/src/ package.yaml stack.yaml


.PHONY: all

all: generate_site

generate_site: ${FONTS} build_site clean_site ./site
	stack $j exec site build


build_site: $(HASKELL_SOURCES)
	stack $j build $(fast) $(build_ghc_opts)

ghci: build_site
	stack $j ghci

fonts: fonts_shell.nix
	@echo 'Installing font package via nix shell'
	@nix-shell fonts_shell.nix --command 'export FONTS_PACKAGE_PATH=$$FONTS_PACKAGE_PATH'
	@echo 'Installing font files from ${FONTS_PACKAGE_PATH}.'
	@mkdir -p ./fonts_
	@find ${FONTS_PACKAGE_PATH} -name 'Inter*.otf' -printf '\t./site/fonts_/%f\n' -exec cp {} ./site/fonts_/ \;
	@chown -R ${USER} ./site/fonts_
	@chmod -R +rw ./site/fonts_

build_hoogle: build_site
	stack hoogle -- generate --local

hoogle: build_hoogle
	stack $j hoogle -- server --local --port=8080

clean: clean_fonts clean_site clean_build

clean_fonts:
	@echo 'Cleaning ./fonts_ folder'
	@chown -R ${USER} ./fonts_
	@chmod -R +rw ./fonts_
	@rm -f ./fonts_/*

clean_site:
	stack exec site clean

clean_build:
	stack clean
