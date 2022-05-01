{ haskellPackages }:
  haskellPackages.callCabal2nix "maiwar" ./. {}
