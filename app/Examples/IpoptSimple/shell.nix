{ nixpkgs ? import <nixpkgs> {} }:
let
  # use the below definition to pin nixpkgs to the 20.09 lts branch if necessary
  # nixpkgs = import (builtins.fetchGit {
  #   url = "https://github.com/nixos/nixpkgs.git";
  #   ref = "nixos-20.09";
  #   rev = "cd63096d6d887d689543a0b97743d28995bc9bc3";
  # }) {};
  mumps = nixpkgs.callPackage ./mumps.nix {};
  ipopt = nixpkgs.callPackage ./ipopt.nix { mumps = mumps; };
in nixpkgs.stdenv.mkDerivation {
    name = "coconut-ipopt";
    nativeBuildInputs = [ nixpkgs.pkg-config ];
    buildInputs = [
      nixpkgs.gnumake
      nixpkgs.coreutils
      nixpkgs.hdf5
      nixpkgs.fftw
      mumps
      ipopt
    ];
    src = ./src;
}
