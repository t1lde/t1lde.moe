{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
    inter_web = import ./InterWeb.nix { inherit lib fetchzip; };
    iosevka_web = import ./IosevkaWeb.nix { inherit lib fetchzip; };
in

pkgs.stdenv.mkDerivation {
    inherit ghc;
    name = "stack_shell";
    buildInputs = [
        inter_web
        iosevka_web
    ];

    shellHook = ''
    export INTER_PACKAGE_PATH=${inter_web}
    export IOSEVKA_PACKAGE_PATH=${iosevka_web}
    '';
}
