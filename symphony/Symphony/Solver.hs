module Symphony.Solver where

import HashedExpression.Embed (version)
import System.Process (readProcess, readProcessWithExitCode)

data AvailableSolver = Ipopt | LBFGSB

-- |
downloadLink :: AvailableSolver -> String
downloadLink solver =
  "https://github.com/McMasterU/HashedExpression/releases/download/v" ++ version ++ "/" ++ solverFileName solver

-- |
solverFileName :: AvailableSolver -> String
solverFileName solver =
  ( case solver of
      Ipopt -> "ipopt"
      LBFGSB -> "lbfgs-b"
  )
    ++ "-"
    ++ version
    ++ ".tar.gz"

-- | TODO: Windows
downloadSolver :: String -> AvailableSolver -> IO ()
downloadSolver outputPath solver = do
  readProcess "curl" ["-LJO", downloadLink solver] "" >>= putStrLn
  readProcess "tar" ["xf", solverFileName solver, "-C", outputPath] "" >>= putStrLn
  readProcess "rm" ["-rf", solverFileName solver] "" >>= putStrLn
