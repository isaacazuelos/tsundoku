let 
  pkgs = import <nixpkgs> {};
in
  {
    tsundoku = pkgs.haskellPackages.callPackage ./default.nix {};
  }
