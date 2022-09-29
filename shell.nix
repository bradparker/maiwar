let
  nixpkgs-source = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/7d226b5d12965b214d299d553fff6a3db7e4202a.tar.gz";
  };
  nixpkgs = import nixpkgs-source {};
  haskellPackages = nixpkgs.haskell.packages.ghc924;
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

