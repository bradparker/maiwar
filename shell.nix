let
  nixpkgs = import <nixpkgs> {};
  haskellPackages = nixpkgs.haskell.packages.ghc921;
  package = import ./. { haskellPackages = haskellPackages; };
in
  haskellPackages.shellFor {
    packages = _: [ package ];
    nativeBuildInputs = [
      haskellPackages.doctest
      haskellPackages.haskell-language-server
      haskellPackages.hlint
    ];
  }

