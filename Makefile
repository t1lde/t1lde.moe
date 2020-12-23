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

generate_site: ${FONTS} ./site/fonts_ build clean_site ./site
	stack exec site build -- $(site_opts)

watch_site: ${FONTS} build clean_site
	stack exec site watch -- $(site_opts)

build: $(HASKELL_SOURCES)
	stack $j build $(fast) $(build_ghc_opts)

ghci: build_site
	stack $j ghci

fonts: clean_fonts
	@echo 'Installing font package via nix shell'
	$(eval INTER_PACKAGE_PATH := $(shell nix-shell fonts_shell.nix --command 'echo $$INTER_PACKAGE_PATH'))
	$(eval IOSEVKA_PACKAGE_PATH := $(shell nix-shell fonts_shell.nix --command 'echo $$IOSEVKA_PACKAGE_PATH'))
	@echo 'Installing font files from ${INTER_PACKAGE_PATH} :'
	@mkdir -p ./site/fonts_/woff2
	@find $(INTER_PACKAGE_PATH) -name 'Inter*.woff' -printf '\t./site/fonts_/%f\n' -exec cp {} ./site/fonts_/ \;
	@find $(INTER_PACKAGE_PATH) -name 'Inter*.woff2' -printf '\t./site/fonts_/%f\n' -exec cp {} ./site/fonts_/ \;
	@find $(INTER_PACKAGE_PATH) -name '*.css' -printf '\t./site/fonts_/%f\n' -exec cp {} ./site/fonts_/ \;
	@echo 'Installing font files from ${IOSEVKA_PACKAGE_PATH} :'
	@find $(IOSEVKA_PACKAGE_PATH) -name '*.woff2' -printf '\t./site/fonts_/woff2/%f\n' -exec cp {} ./site/fonts_/woff2/ \;
	@find $(IOSEVKA_PACKAGE_PATH) -name '*.css' -printf '\t./site/fonts_/%f\n' -exec cp {} ./site/fonts_/ \;
	@chown -R ${USER} ./site/fonts_
	@chmod -R +rw ./site/fonts_

build_hoogle: haddock
	stack hoogle -- generate --local

hoogle: build_hoogle
	stack $j hoogle -- server --local --port=8080

haddock:
	stack $j haddock --dependencies-only

clean: clean_fonts clean_build

clean_fonts: fonts_shell.nix InterWeb.nix
	@echo 'Cleaning ./fonts_ folder'
	@chown -R ${USER} ./site/fonts_
	@chmod -R +rw ./site/fonts_
	@rm -rf ./site/fonts_/*

clean_site:
	stack exec site clean

clean_build:
	stack clean
