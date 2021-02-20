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

HASKELL_SOURCES := site/src/ HakyllSite.cabal

.PHONY: all

all: generate_site

generate_site: ${FONTS} ./site/fonts_ build clean_site ./site
	cabal exec site build -- $(site_opts)

watch_site: ${FONTS} build clean_site
	cabal exec site watch -- $(site_opts)

build: $(HASKELL_SOURCES) cabal2nix
	cabal build $(build_ghc_opts)

cabal2nix: hpack
	cabal2nix . > HakyllSite.nix

hpack: package.yaml HakyllSite.cabal
	hpack

ghci: build_site
	cabal $j repl

fonts: clean_fonts
	@echo 'Installing font package via nix shell'
	$(eval INTER_PACKAGE_PATH := $(shell nix-shell -A inter deps_shell.nix --command 'echo $$INTER_PACKAGE_PATH'))
	$(eval IOSEVKA_PACKAGE_PATH := $(shell nix-shell -A iosevka deps_shell.nix --command 'echo $$IOSEVKA_PACKAGE_PATH'))
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

aoc: clean_aoc
	@echo "Retrieving AOC2020 Repo via nix shell"
	$(eval AOC_PACKAGE_PATH := $(shell nix-shell -A aoc2020 deps_shell.nix --command 'echo $$AOC_PACKAGE_PATH'))
	@echo "Copying repo from ${AOC_PACKAGE_PATH}"
	@mkdir -p ./site/deps_/
	cp -r $(AOC_PACKAGE_PATH)/AOC/ ./site/deps_/
	chown -R ${USER} ./site/deps_/
	chmod -R +rw ./site/deps_/


haddock:
	cabal $j haddock --dependencies-only

clean: clean_fonts clean_build

clean_fonts: deps_shell.nix InterWeb.nix IosevkaWeb.nix
	@echo 'Cleaning ./site/fonts_ folder'
	@-chown -R ${USER} ./site/fonts_
	@-chmod -R +rw ./site/fonts_
	@-rm -rf ./site/fonts_/*

clean_aoc: deps_shell.nix
	@echo 'Cleaning ./site/deps_/AOC folder'
	@-chown -R ${USER} /site/deps_/AOC
	@-chmod -R +rw ./site/deps_/AOC
	@-rm -rf ./site/deps_/AOC/


clean_site:
	cabal exec site clean

clean_build:
	cabal clean
