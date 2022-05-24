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
      #url = "github:con-kitty/categorifier/master";
      url =
        # pull request categorifier#66
        "github:con-kitty/categorifier/d48b7bb9c45c95f1d4f2f46641d483cc4f401ca8";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.concat.follows = "concat";
    };
    connections = {
      url = "github:cmk/connections/master";
      flake = false;
    };
    TypeCompose = {
      url = "github:conal/TypeCompose/daf13efa6e6b37960de98146bab4a061462fc22b";
      flake = false;
    };
    sbv = {
      url = "github:LeventErkok/sbv/v9.0";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, concat, categorifier, connections
    , TypeCompose, sbv }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        overlay_deps = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (self: super: {
                # zliu41's fix for GHC 9.2
                "TypeCompose" =
                  self.callCabal2nix "TypeCompose" TypeCompose { };
                "connections" =
                  self.callCabal2nix "connections" connections { };
                # test is broken with DBool.
                "generic-accessors" =
                  haskellLib.dontCheck super.generic-accessors;
                # sbv-9.0, bypassing checkPhase that takes too long.
                "sbv" = haskellLib.dontCheck (self.callCabal2nix "sbv" sbv { });
              });
          });
        };

        haskellLib = (import nixpkgs { inherit system; }).haskell.lib;

        # random-expressions test misses a CLI option.
        noCheckPackages = [ "categorifier-c-tests" ];

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
            value = let
              p = self.callCabal2nix name (./. + "/${path}") { };
              p1 = if builtins.elem name noCheckPackages then
                haskellLib.dontCheck p
              else
                p;
            in p1;
          }) categorifierCPackages);

        # see these issues and discussions:
        # - https://github.com/NixOS/nixpkgs/issues/16394
        # - https://github.com/NixOS/nixpkgs/issues/25887
        # - https://github.com/NixOS/nixpkgs/issues/26561
        # - https://discourse.nixos.org/t/nix-haskell-development-2020/6170
        fullOverlays = [
          overlay_deps
          (final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides =
                final.lib.composeExtensions (old.overrides or (_: _: { }))
                haskellOverlay;
            });
          })
        ];

      in {
        packages = let
          packagesOnGHC = ghcVer:
            let
              overlayGHC = final: prev: {
                haskellPackages = prev.haskell.packages.${ghcVer};
              };

              newPkgs = import nixpkgs {
                overlays = [ overlayGHC (concat.overlay.${system}) ]
                  ++ (categorifier.overlays.${system}) ++ fullOverlays;
                inherit system;
                config.allowBroken = true;
              };

              individualPackages = builtins.listToAttrs (builtins.map (p: {
                name = ghcVer + "_" + p;
                value = builtins.getAttr p newPkgs.haskellPackages;
              }) categorifierCPackageNames);

              allEnv = let
                hsenv = newPkgs.haskellPackages.ghcWithPackages (p:
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
        // packagesOnGHC "ghc901" // packagesOnGHC "ghc921"
        // packagesOnGHC "ghcHEAD";

        overlays = fullOverlays;

        devShells = let
          mkPkgs = ghcVer:
            let
              overlayGHC = final: prev: {
                haskellPackages = prev.haskell.packages.${ghcVer};
              };
            in import nixpkgs {
              overlays = [ overlayGHC (concat.overlay.${system}) ]
                ++ (categorifier.overlays.${system}) ++ fullOverlays;
              inherit system;
              config.allowBroken = true;
            };

          mkDevShell = ghcVer:
            let pkgs = mkPkgs ghcVer;
            in pkgs.haskellPackages.shellFor {
              packages = ps:
                builtins.map (name: ps.${name}) categorifierCPackageNames;
              buildInputs =
                # use nixpkgs default tools
                [
                  pkgs.haskell.packages.ghc8107.cabal-install
                  pkgs.haskell.packages.ghc8107.hlint
                ] ++
                # haskell-language-server on GHC 9.2.1 is broken yet.
                pkgs.lib.optional (ghcVer != "ghc921")
                [ pkgs.haskell-language-server ];
              withHoogle = false;
            };

          mkUserShell = ghcVer:
            let
              pkgs = mkPkgs ghcVer;
              hsenv = pkgs.haskellPackages.ghcWithPackages
                (ps: builtins.map (name: ps.${name}) categorifierCPackageNames);
            in pkgs.mkShell {
              buildInputs = [ hsenv pkgs.haskellPackages.cabal-install ] ++
                # haskell-language-server on GHC 9.2.1 is broken yet.
                pkgs.lib.optional (ghcVer != "ghc921")
                [ pkgs.haskell-language-server ];
            };

        in {
          # nix develop .#ghc8107
          # (or .#ghc901 .#ghc921)
          # This is used for building categorifier-c
          "default" = mkDevShell "ghc901";
          "ghc8107" = mkDevShell "ghc8107";
          "ghc901" = mkDevShell "ghc901";
          "ghc921" = mkDevShell "ghc921";
          # The shell with all batteries included!
          "user-shell" = mkUserShell "ghc8107";
        };
      });
}
