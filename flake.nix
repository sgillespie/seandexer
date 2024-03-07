{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;

      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      perSystem = { self', system, lib, config, pkgs, ...}: {
        haskellProjects.default = {
          projectFlakeName = "seandexer";

          devShell = {
            enable = true;

            tools = hsPkgs: {
              inherit (hsPkgs)
                cabal-install fourmolu haskell-language-server hlint;
              inherit (pkgs) nixpkgs-fmt;
            };
          };
        };

        treefmt.config = {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
          };
        };

        packages.default = self'.packages.seandexer;
      };
    };
}
