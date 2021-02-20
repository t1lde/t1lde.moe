---
title: This Site
---
<article>

This website is a static website generated using [Hakyll](https://jaspervdj.be/hakyll/), a static site generator library for Haskell. 

I am not especially a front-end specialist, but I hope you think it looks good anyway :) .

## Gitlab Repo

You can find the source code for my site in the [Gitlab](https://gitlab.com/t1lde/t1lde.moe) repository, which also contains some utilities which I needed to build the site.

## Cabal2nix

The site project is built using cabal, with [nix](https://nixos.org/) to manage the dependencies with the help of [cabal2nix](https://github.com/NixOS/cabal2nix). 

Haskell library dependencies, the font packages and [Advent of Code](/AOC2020/AdventOfCode.html) Literate Haskell source are all managed using nix packages.

A hacky but functional Makefile calling ``nix-shell`` puts all the pieces in place ready for site generation and deployment.

## Fonts

[Inter](https://rsms.me/inter/) is used for the main text font, and [Iosevka](https://github.com/be5invis/Iosevka) for monospaced code.
Both are free and open source, and quite convenient to grab as webfonts using a modified version of the nix packages for each. 


</article>

