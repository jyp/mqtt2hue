{ nixpkgs ? import <nixpkgs> {} }:
let
  # nixpkgs_source = fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz;
  nixpkgs_source = fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz;
  # nixpkgs_source = fetchTarball https://github.com/NixOS/nixpkgs/archive/fcab19deb78fbb5ea24e19b133cf34358176396a.tar.gz;
  overlays = [];
  myNix = import nixpkgs_source {inherit overlays; };
in
with myNix.pkgs; 
let hpDef = haskellPackages.override{
      overrides = self: super: {
        # pretty-compact = self.callPackage ./pretty-compact.nix {};
        # typedflow = self.callPackage ./typedflow.nix {};
      };};
    hp = hpDef;
    ghc = hp.ghcWithPackages (ps: with ps; ([ cabal-install servant servant-server aeson warp-tls lucid wai-enforce-https hashable servant-xml large-hashable ]));
in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ glibcLocales ghc ];
  shellHook = ''
    export LANG=en_US.UTF-8
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}


