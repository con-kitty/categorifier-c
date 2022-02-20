with import ./nixpkgs.nix {};

mkShell {
  buildInputs = [
    clang
    (haskellPackages.ghcWithPackages (p: with p; [
      cabal-install
    ]))
    ormolu # for Haskell formatting
  ];
}
