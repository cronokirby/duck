{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs) {
          inherit system;
        };

        overlay = final: prev: {
          duck = final.callCabal2nix "duck" ./. {};
        };

        haskellPackages = pkgs.haskellPackages.extend overlay;
      in rec {
        defaultPackage = pkgs.buildPackages.writeShellScriptBin "duck" ''
          exec env PATH=${pkgs.gh}/bin:$PATH ${haskellPackages.duck}/bin/duck "$@"
        '';

        devShell = haskellPackages.shellFor {
          packages = p : [
            p.duck
          ];
          nativeBuildInputs = [
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
          ];
          buildInputs = [
            pkgs.gh
          ];
        };
      }
    );
}
