let
  pkgs = import <nixpkgs> {};
  highs = pkgs.callPackage ./highs.nix {debug = false;};
in
pkgs.mkShell {
  # "override" required dependencies
  buildInputs = ([
    highs
    pkgs.gmp
    pkgs.pkg-config
    pkgs.zlib
    pkgs.gcc
    pkgs.perl
  ]);
  shellHook = ''
    # required for running HSC C programs - they seem not to ask pkg-config
    export LD_LIBRARY_PATH=${pkgs.stdenv.cc.cc.lib}/lib:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
    # Why are these paths not set by Nix?
    export PKG_CONFIG_PATH=${highs}/lib/pkgconfig:$PKG_CONFIG_PATH
    export LD_LIBRARY_PATH=${highs}/lib:$LD_LIBRARY_PATH
    export PATH=${highs}/bin:$PATH
  '';
}
