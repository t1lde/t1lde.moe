{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/9518fac712ca001009bd12a3c94621f1ee805657.tar.gz") {}, ghc }:

with nixpkgs;

haskell.lib.buildStackProject {
    inherit ghc;
    name = "stack_shell";
    buildInputs = [
      pkgs.haskellPackages.hakyll
      pkgs.pandoc
      pkgs.zlib
    ];
}
