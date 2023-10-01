let
  nixpkgs_source = fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz;
  # nixpkgs_source = <nixpkgs>;
in { nixpkgs ? import nixpkgs_source {}  }:
nixpkgs.pkgs.haskellPackages.callPackage ./mqtt2hue.nix { }
