{ lib, stdenv, pkgconfig, glpk, fetchurl }:

stdenv.mkDerivation rec {
  pname = "glpk";
  version = "5.0";

  src = fetchurl {
    url = "https://ftp.gnu.org/gnu/glpk/glpk-${version}.tar.gz";
    sha256 = "05bgxidxj8d9xdp82niy7cy36w181cxq7p8vc3y2ixshpgp1642a";
  };

  CXXDEFS = [ "-DHAVE_RAND" "-DHAVE_CSTRING" "-DHAVE_CSTDIO" ];

  configureFlags = [

  ];

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [  ];

  enableParallelBuilding = true;
}
