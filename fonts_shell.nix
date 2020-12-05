{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
    inter_web = import ./InterWeb.nix { inherit lib fetchzip; };
in

pkgs.stdenv.mkDerivation {
    inherit ghc;
    name = "stack_shell";
    buildInputs = [
      inter_web
    ];

    shellHook = ''
    export FONTS_PACKAGE_PATH=${inter_web}
    '';
}
