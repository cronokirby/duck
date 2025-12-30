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

        project = pkgs.haskellPackages.callCabal2nix "duck" ./. {};
      in rec {
        defaultPackage = project;

        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            haskellPackages.ghc
            haskellPackages.cabal-install
          ];

          buildInputs = [
            project
          ];
        };
      }
    );
}
