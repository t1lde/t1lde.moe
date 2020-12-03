{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

pkgs.stdenv.mkDerivation {
    inherit ghc;
    name = "stack_shell";
    buildInputs = [
      pkgs.inter
    ];

    shellHook = ''
    export FONTS_PACKAGE_PATH=${pkgs.inter}
    '';
}
