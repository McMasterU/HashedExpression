{ nixpkgs ? import (builtins.fetchTarball {
  name = "nixpkgs-unstable-2023-04-28";
  url = "https://github.com/NixOS/nixpkgs/archive/d6b863fd9b7bb962e6f9fdf292419a775e772891.tar.gz";
  sha256 = "02rd1n6d453rdp2978bvnp99nyfa26jxgbsg78yb9mmdxvha3hnr";
  }) {} }:

let
  mumps = nixpkgs.callPackage ./ipopt/mumps.nix {};
  ipopt = nixpkgs.callPackage ./ipopt/ipopt.nix { mumps = mumps; };
  glpk = nixpkgs.callPackage ./glpk/glpk.nix {};
  highs = nixpkgs.callPackage ./highs/highs.nix {debug = false;};
in nixpkgs.haskell.lib.buildStackProject {
  name = "HashedExpression";
  nativeBuildInputs = [ nixpkgs.pkg-config ];
  buildInputs = [
      nixpkgs.hello
      nixpkgs.haskell.compiler.ghc902
		  nixpkgs.ghcid
		  nixpkgs.cabal-install
		  nixpkgs.zlib
		  nixpkgs.graphviz
		  nixpkgs.haskellPackages.ormolu
		  nixpkgs.haskellPackages.hoogle
		  nixpkgs.haskellPackages.zlib
		  nixpkgs.haskellPackages.hlint
      nixpkgs.haskellPackages.nlopt-haskell
		  (nixpkgs.haskell-language-server.override { supportedGhcVersions = [ "902" ]; })
		  nixpkgs.gnumake
		  nixpkgs.coreutils
		  nixpkgs.hdf5
		  nixpkgs.fftw
		  nixpkgs.gcc
      nixpkgs.mpi
      nixpkgs.openssh
      nixpkgs.nlopt
      nixpkgs.blas
      nixpkgs.lapack
		  ipopt
		  mumps
		  glpk
      highs
		];
  STACK_IN_NIX_EXTRA_ARGS
  = " --extra-lib-dirs=${nixpkgs.nlopt}/lib" 
    + " --extra-include-dirs=${nixpkgs.nlopt}/include" 
  ;
}

  # To use this add the following to the bottom of your stack.yaml
  # nix:
  #   enable: true
  #   shell-file: shell.nix

  # also execute the following in the project root
  # echo "use nix" >> .envrc
  # direnv allow
