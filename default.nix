{nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19db3e5ea2777daa874563b5986288151f502e27.tar.gz") { }, ghc ? nixpkgs.ghc}:

with nixpkgs;
let

  shell = pkgs.haskell.lib.buildStackProject {
    name = "hakyll_site";
    buildInputs = [
        pkgs.haskellPackages.hakyll
        pkgs.zlib
    ];
    shellHook = ''
    rm -rf ./fonts/*
    mkdir ./fonts
    cp -r ${pkgs.inter}/share/fonts/* ./fonts/
    '';
    inherit ghc;
  };

in
{
  inherit shell;
}
