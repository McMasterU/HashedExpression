{ lib, stdenv, fetchurl, blas, lapack, gfortran, metis, gnupatch, pkgconfig }:

stdenv.mkDerivation {
  name = "mumps";
  version = "4.10.0";

  srcs =[
    (fetchurl {
      url = https://github.com/coin-or-tools/ThirdParty-Mumps/archive/refs/tags/releases/1.6.2.tar.gz;
      sha256 = "0pc9v2mry4hswyxr39nfb45zrlbdyllmcjcwmlwrabdlkh5x7z8c";
    })
    (fetchurl {
      url = https://coin-or-tools.github.io/ThirdParty-Mumps/MUMPS_4.10.0.tar.gz;
      sha256 = "0cyr0p5aahsvpab0il562dprff13xsf9rgpi5xxa2lacly8nzy6h";
    })
  ];

  sourceRoot = "ThirdParty-Mumps-releases-1.6.2";

  unpackPhase = ''
                runHook preUnpack

                for _src in $srcs
                do
                  tar -xvf $_src
                done
                mv MUMPS_4.10.0 "ThirdParty-Mumps-releases-1.6.2"/MUMPS

                cd "ThirdParty-Mumps-releases-1.6.2"
                patch -p0 < mumps.patch
                patch -p0 < mumps_mpi.patch
                mv MUMPS/libseq/mpi.h MUMPS/libseq/mumps_mpi.h
                cd ..

                runHook postUnpack
                '';

  nativeBuildInputs = [
    pkgconfig
  ];

  buildInputs = [
    gfortran
    lapack
    blas
    metis
    gnupatch
  ];
}
