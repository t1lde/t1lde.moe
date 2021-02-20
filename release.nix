let

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs20_09.json);

  # Hakyll/Pandoc in nixos are fussy about versions
  nixpkgs2 = builtins.fromJSON (builtins.readFile ./nixpkgs20_03.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  src2 = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs2) rev sha256;
  };

  pkgs = import src {  };

  pkgs2 = import src2 { };

  hakyll = pkgs.haskellPackages.callPackage ./hakyll.nix { pandoc=pkgs2.pandoc; };

  AOC2020 = pkgs.haskellPackages.callPackage ./AOC2020.nix {};


in

{
    hakyllsite = pkgs.haskellPackages.callPackage ./HakyllSite.nix {
      inherit hakyll;
    };

  }
