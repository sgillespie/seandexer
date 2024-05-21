{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";

    chap = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };

  };

  outputs = inputs@{ self, nixpkgs, utils, haskellNix, iohkNix, ... }:
    let
      supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin" "aarch64-linux"];
    in
      utils.lib.eachSystem supportedSystems (system:
        let
          overlays = builtins.attrValues (iohkNix.overlays) ++ [
            haskellNix.overlay

            (final: prev: {
              seandexer = final.haskell-nix.cabalProject' {
                src = ./.;
                compiler-nix-name = "ghc98";

                # Add chap to input map, so we can find cardano packages
                inputMap = {
                  "https://chap.intersectmbo.org/" = inputs.chap;
                };

                shell = {
                  # Tools from Hackage
                  tools = {
                    cabal = {};
                    hlint = {};
                    fourmolu = {};

                    haskell-language-server = {
                      src = inputs.haskellNix.inputs."hls-2.8";
                    };
                  };

                  # Non-haskell tools
                  buildInputs = with pkgs; [
                    nixpkgs-fmt
                    treefmt
                  ];
                };
              };
            })
          ];

          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };

          inherit (pkgs) lib;

          flake = pkgs.seandexer.flake {};
        in
          pkgs.lib.recursiveUpdate flake {
            # Attrs we want to add to the flake
            packages = {
              default = flake.packages."seandexer:exe:seandexer";
              checks = flake.checks."seandexer:test:seandexer-test";
            };
          });

  nixConfig = {
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "sgillespie.cachix.org-1:Zgif/WHW2IzHqbMb1z56cMmV5tLAA+zW9d5iB5w/VU4="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    substituters = [
      "https://cache.nixos.org/"
      "https://sgillespie.cachix.org"
      "https://cache.iog.io"
    ];

    allow-import-from-derivation = "true";
    experimental-features = [ "nix-command flakes" ];
  };
}
