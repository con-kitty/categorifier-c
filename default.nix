with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    clang
    (haskell.packages.ghc8107.ghcWithPackages (p: with p; [
      cabal-install
    ]))
    ormolu # for Haskell formatting
  ];
}
