j ?= -j 6

ifndef NoFast
fast = --fast
endif

ifdef build_args
build_ghc_opts = --ghc-options  $(build_args)
endif

ifndef NoNix
FONTS=fonts
endif

HASKELL_SOURCES := ./src ./app site/src/ package.yaml stack.yaml


.PHONY: all

all: generate_site

generate_site: ${FONTS} ./site/fonts_ build_site clean_site ./site
	stack exec site build

watch_site: ${FONTS} build_site clean_site
	stack exec site watch

build_site: $(HASKELL_SOURCES)
	stack $j build $(fast) $(build_ghc_opts)

ghci: build_site
	stack $j ghci

fonts: clean_fonts
	@echo 'Installing font package via nix shell'
	$(eval FONTS_PACKAGE_PATH := $(shell nix-shell fonts_shell.nix --command 'echo $$FONTS_PACKAGE_PATH'))
	@echo 'Installing font files from ${FONTS_PACKAGE_PATH} :'
	@mkdir -p ./site/fonts_
	@find $(FONTS_PACKAGE_PATH) -name 'Inter*.woff' -printf '\t./site/fonts_/%f\n' -exec cp {} ./site/fonts_/ \;
	@find $(FONTS_PACKAGE_PATH) -name 'Inter*.woff2' -printf '\t./site/fonts_/%f\n' -exec cp {} ./site/fonts_/ \;
	@find $(FONTS_PACKAGE_PATH) -name '*.css' -printf '\t./site/fonts_/%f\n' -exec cp {} ./site/fonts_/ \;
	@chown -R ${USER} ./site/fonts_
	@chmod -R +rw ./site/fonts_

build_hoogle: build_site
	stack hoogle -- generate --local

hoogle: build_hoogle
	stack $j hoogle -- server --local --port=8080

clean: clean_fonts clean_build

clean_fonts: fonts_shell.nix InterWeb.nix
	@echo 'Cleaning ./fonts_ folder'
	@chown -R ${USER} ./site/fonts_
	@chmod -R +rw ./site/fonts_
	@rm -f ./site/fonts_/*

clean_site:
	stack exec site clean

clean_build:
	stack clean
