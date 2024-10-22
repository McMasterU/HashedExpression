{
debug ? false,
fetchFromGitHub, gcc, cmake, zlib,
stdenv, lib
}:

stdenv.mkDerivation rec {
  pname = "highs";
  version = "1.6.0";

  src = fetchFromGitHub {
    owner = "ERGO-Code";
    repo = "HiGHS";
    rev = "df7731869ba1100ec8034ae5dc86c29cc4a98c40"; # branch "latest"
    sha256 = "sha256-XaExeKCzDwd7lCpItqYn6w57c2k+XGul7r6RqdY2PTM=";
  };

  separateDebugInfo = debug;
  dontStrip = debug;
  patches = lib.optionals debug [ ./enable-debugging.patch ];
  cmakeBuildType =
    if debug
      then "RelWithDebInfo"
      else "Release";

  buildInputs = [ gcc cmake zlib ];

  meta = with lib; {
    description = "Open source serial and parallel solvers for large-scale sparse linear programming";

    longDescription =
      ''
         HiGHS can solve linear programming (LP) models
         as well as mixed integer linear programming (MILP) models,
         for which some of the variables must take integer values.

         HiGHS also solves quadratic programming (QP) models,
         which contain an additional objective term,
         where the Hessian matrix is positive semi-definite.
         HiGHS cannot solve QP models
         where some of the variables must take integer values.
      '';

    homepage = "https://highs.dev/";
    downloadPage = "https://ergo-code.github.io/HiGHS/";
    license = licenses.mit;

    maintainers = with maintainers; [ thielema ];
    mainProgram = "highs";
    platforms = platforms.all;
  };
}
