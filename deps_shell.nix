{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
    inter_web = import ./InterWeb.nix { inherit lib fetchzip; };
    iosevka_web = import ./IosevkaWeb.nix { inherit lib fetchzip; };
    aoc_src = fetchgit {
        url = "https://github.com/t1lde/AOC2020/";
        rev = "f43e05520acef90fdf678038d752229de158870e";
        sha256 = "1hvb723psbhvc0nsk0lg52fi47g9y5qf882lmmgqzs4m1kjql7gs";
        fetchSubmodules = false;
    };

in

{
    inter =
        pkgs.stdenv.mkDerivation {
            name = "inter_shell";
            buildInputs = [
                inter_web
            ];

            shellHook = ''
            export INTER_PACKAGE_PATH=${inter_web}
            '';
        };

    iosevka =
        pkgs.stdenv.mkDerivation {
            name = "iosevka_shell";
            buildInputs = [
                iosevka_web
            ];
            shellHook = ''
            export IOSEVKA_PACKAGE_PATH=${iosevka_web}
            '';
        };

    aoc2020 =
        pkgs.stdenv.mkDerivation {
            name = "aoc_shell";
            buildInputs = [
            ];
            shellHook = ''
            export AOC_PACKAGE_PATH=${aoc_src}
            '';
        };



}
