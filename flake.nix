{
  description = "categorifier-c";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
    concat = {
      url = "github:con-kitty/concat/wavewave-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utlis.follows = "flake-utils";
    };
    categorifier = {
      url = "github:con-kitty/categorifier/wavewave-flakes-2";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.concat.follows = "concat";
    };
    connections = {
      url = "github:cmk/connections/master";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, concat, categorifier, connections }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlay_connection = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                "connections" =
                  self.callCabal2nix "connections" connections { };
                # test is broken with DBool.
                "generic-accessors" =
                  haskellLib.dontCheck super.generic-accessors;
              });
          });
        };

        haskellLib = (import nixpkgs { inherit system; }).haskell.lib;
        parseCabalProject = import (categorifier + "/parse-cabal-project.nix");
        categorifierCPackages =
          # found this corner case
          [{
            name = "categorifier-c";
            path = ".";
          }] ++ parseCabalProject ./cabal.project;
        categorifierCPackageNames =
          builtins.map ({ name, ... }: name) categorifierCPackages;

        haskellOverlay = self: super:
          builtins.listToAttrs (builtins.map ({ name, path }: {
            inherit name;
            value = self.callCabal2nix name (./. + "/${path}") { };
          }) categorifierCPackages);

      in {
        packages = let
          packagesOnGHC = ghcVer:
            let
              overlayGHC = final: prev: {
                haskellPackages = prev.haskell.packages.${ghcVer};
              };

              newPkgs = import nixpkgs {
                overlays = [
                  overlayGHC
                  overlay_connection
                  (concat.overlay.${system})
                  (categorifier.overlay.${system})
                ];
                inherit system;
                config.allowBroken = true;
              };

              newHaskellPackages = newPkgs.haskellPackages.override (old: {
                overrides =
                  newPkgs.lib.composeExtensions (old.overrides or (_: _: { }))
                  haskellOverlay;
              });

              individualPackages = builtins.listToAttrs (builtins.map (p: {
                name = ghcVer + "_" + p;
                value = builtins.getAttr p newHaskellPackages;
              }) categorifierCPackageNames);

              allEnv = let
                hsenv = newHaskellPackages.ghcWithPackages (p:
                  let
                    deps = builtins.map ({ name, ... }: p.${name})
                      categorifierCPackages;
                  in deps);
              in newPkgs.buildEnv {
                name = "all-packages";
                paths = [ hsenv ];
              };
            in individualPackages // { "${ghcVer}_all" = allEnv; };

        in packagesOnGHC "ghc8107" // packagesOnGHC "ghc884"
        // packagesOnGHC "ghc901" // packagesOnGHC "ghc921";

        # see these issues and discussions:
        # - https://github.com/NixOS/nixpkgs/issues/16394
        # - https://github.com/NixOS/nixpkgs/issues/25887
        # - https://github.com/NixOS/nixpkgs/issues/26561
        # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
        overlays = [
          overlay_connection
          (final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides =
                final.lib.composeExtensions (old.overrides or (_: _: { }))
                haskellOverlay;
            });
          })
        ];

        devShells = let
          ghcVer = "ghc8107";
          overlayGHC = final: prev: {
            haskellPackages = prev.haskell.packages.${ghcVer};
          };
          pkgs = import nixpkgs {
            overlays = [
              overlayGHC
              overlay_connection
              (concat.overlay.${system})
              (categorifier.overlay.${system})
            ];
            inherit system;
            config.allowBroken = true;
          };

        in {
          # Default shell invoked by nix develop .#
          # This is used for building categorifier-c
          default = let
            hsenv = pkgs.haskellPackages.ghcWithPackages (p: [
              p.cabal-install
              p.categorifier-category
              p.categorifier-client
              p.categorifier-common
              p.categorifier-concat-extensions-category
              p.categorifier-concat-extensions-integration
              p.categorifier-concat-integration
              p.categorifier-duoids
              p.categorifier-ghc
              p.categorifier-hedgehog
              p.categorifier-plugin
              p.categorifier-th
              p.categorifier-unconcat-category
              p.categorifier-unconcat-integration
              p.concat-classes
              p.connections
            ]);
          in pkgs.mkShell {
            buildInputs = [ hsenv pkgs.haskell-language-server ];
          };
          # The shell with all batteries included!
          user-shell = let
            postBuildHaskellPackages = pkgs.haskellPackages.override (old: {
              overrides =
                pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
                haskellOverlay;
            });
            hsenv = postBuildHaskellPackages.ghcWithPackages (p: [
              p.cabal-install
              p.categorifier-c
              p.categorifier-c-examples
              p.categorifier-c-hk-classes
              p.categorifier-c-maker-map
              p.categorifier-c-recursion
              p.categorifier-c-unconcat
              p.categorifier-c-test-lib
            ]);
          in pkgs.mkShell {
            buildInputs = [ hsenv pkgs.haskell-language-server ];
          };
        };
      });
}
