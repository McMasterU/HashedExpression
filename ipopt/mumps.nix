{ lib, stdenv, fetchurl, blas, lapack, gfortran, metis, gnupatch, mpi, pkgconfig }:

stdenv.mkDerivation {
  name = "mumps";
  version = "5.5.0";

  srcs =[
    (fetchurl {
      url = https://github.com/coin-or-tools/ThirdParty-Mumps/archive/refs/tags/releases/3.0.3.tar.gz;
      sha256 = "sha256-D1ZWaJCRd1FpHytjOxd2Wsx34KIgXOA03D42KqabEYo=";
    })
    (fetchurl {
      url = https://coin-or-tools.github.io/ThirdParty-Mumps/MUMPS_5.5.0.tar.gz;
      sha256 = "sha256-5U0XxeQqNsQGB6AyeeBwTSOdcdOFA6q2jvO/4KmnnBM=";
    })
  ];

  sourceRoot = "ThirdParty-Mumps-releases-3.0.3";

  unpackPhase = ''
                runHook preUnpack

                for _src in $srcs
                do
                  tar -xvf $_src
                done
                mv MUMPS_5.5.0 "ThirdParty-Mumps-releases-3.0.3"/MUMPS

                cd "ThirdParty-Mumps-releases-3.0.3"
                patch -p0 < mumps_mpi.patch
                mv MUMPS/libseq/mpi.h MUMPS/libseq/mumps_mpi.h
                cd ..

                runHook postUnpack
                '';

  nativeBuildInputs = [
    pkgconfig
    mpi
  ];

  buildInputs = [
    gfortran
    lapack
    blas
    metis
    gnupatch
  ];
}
