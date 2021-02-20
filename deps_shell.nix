{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
    inter_web = import ./InterWeb.nix { inherit lib fetchzip; };
    iosevka_web = import ./IosevkaWeb.nix { inherit lib fetchzip; };
    aoc_src = fetchgit {
        url = "https://github.com/t1lde/AOC2020";
        sha256 = "0wdk9xqxz7lyawrhr6c7r8jym9xs06qda78cw27khsamka58zq20";
        rev = "9aaa0346234d4cf03043d2a8cd834e1102be3e02";
        fetchSubmodules = true;
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
