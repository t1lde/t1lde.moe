{nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19db3e5ea2777daa874563b5986288151f502e27.tar.gz") { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

let
  f = pkgs.haskell.lib.buildStackProject {
    name = "hakyll_site";
    buildInputs = [
        pkgs.haskellPackages.hakyll
        pkgs.zlib
        pkgs.inter
    ];
    inherit ghc;
  };

  drv = haskellPackages.callPackage f {};

in

if pkgs.lib.inNixShell then drv.env else drv
