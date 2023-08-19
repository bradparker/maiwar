let
  nixpkgs-source = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/4fe4a74fe55fcb512902bbdee59a7fdcc9f7c754.tar.gz";
  };
  nixpkgs = import nixpkgs-source {};
  haskellPackages = nixpkgs.haskellPackages;
  package = import ./. { inherit haskellPackages; };
in
  haskellPackages.shellFor {
    packages = _: [ package ];
    nativeBuildInputs = [
      haskellPackages.doctest
      haskellPackages.haskell-language-server
      haskellPackages.hlint
    ];
  }

