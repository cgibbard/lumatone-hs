let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./lumatone-hs.nix {}
