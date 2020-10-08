{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module CSimpleSpec where

import Commons
import Data.Array
import Data.List (tails)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (ExitCode (..))
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Embed
import HashedExpression.Internal
import HashedExpression.Internal.Base hiding ((<.>))
import HashedExpression.Internal.Node
import HashedExpression.Interp
import HashedExpression.Utils
import HashedExpression.Value
import System.FilePath
import System.Process (readProcess, readProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck
import Prelude hiding ((!!))

-- | Compute the inner offset:
-- e.g localOffset [3, 4, 5] [2, 0, 1] = 2 * (4 * 5) + 0 * (5) + 1
localOffset :: [Int] -> [Int] -> Int
localOffset shape indices
  | length shape == length indices =
    sum . zipWith (*) indices . map product . tail . tails $ shape
  | otherwise = error $ "shape and indices are not compatible" ++ show shape ++ show indices

-- | Generate a fully working C program that compute the expression and
--   print out the result
singleExpressionCProgram :: IsExpression e => ValMap -> e -> Code
singleExpressionCProgram valMap e =
  [ "#include <math.h>", --
    "#include <stdio.h>",
    "#include <stdlib.h>",
    "#include <complex.h>",
    if containsFTNode $ mp
      then T.pack $ fftUtils
      else "",
    "int main()" --
  ]
    ++ scoped
      ( initMemory
          ++ fromCCode assigningValues
          ++ [codes]
          ++ fromCCode printValue
          ++ fromCCode releaseMemory
      )
  where
    (mp, n) = asRawExpr e
    (shape, et, op) = retrieveNode n mp
    bound = product shape
    codegen@CSimpleCodegen {..} = initCodegen CSimpleConfig {output = OutputText, maxIteration = Nothing} mp []
    [i, j, k, nooffset] = ["i", "j", "k", "0"]
    initMemory :: Code
    initMemory =
      concatMap
        fromCCode
        [ "double *ptr" := (fun "malloc" ["sizeof(double) * " <> tt totalReal]),
          "double complex *ptr_c" := (fun "malloc" ["sizeof(double complex) * " <> tt totalComplex])
        ]
    -- codes to compute
    codes = evaluating codegen [n]
    -- print the value of expression
    printValue :: CCode
    printValue
      | et == R =
        Scoped
          [ Printf [T.intercalate " " $ map tt shape],
            Printf ["\\n"],
            for i bound [Printf ["%f ", n !! i]]
          ]
      | et == C =
        Scoped
          [ Printf [T.intercalate " " $ map tt shape],
            Printf ["\\n"],
            for i bound [Printf ["%f ", fun "creal" [n !! i]]],
            Printf ["\\n"],
            for i bound [Printf ["%f ", fun "cimag" [n !! i]]]
          ]
    releaseMemory = Statement "free(ptr)"
    -------------------------------------------------------------------------------
    assigningValues :: CCode
    assigningValues = Scoped $ map assignValue names
      where
        [i, j, k, nooffset] = ["i", "j", "k", "0"]
        names :: [(String, NodeID)]
        names = varsWithNodeID mp ++ paramsWithNodeID mp
        assignValue :: (String, NodeID) -> CCode
        assignValue (name, n) =
          case Map.lookup name valMap of
            Just (VScalar val) -> (n !! nooffset) := (tt val)
            Just (V1D array1d) ->
              let assignIndex id = (n !! tt id) := (tt (array1d ! id))
               in Scoped $ map assignIndex $ indices array1d
            Just (V2D array2d) ->
              let shape = retrieveShape n mp
                  assignIndex (id1, id2) = (n !! tt (localOffset shape [id1, id2])) := (tt (array2d ! (id1, id2)))
               in Scoped $ map assignIndex $ indices array2d
            Just (V3D array3d) ->
              let shape = retrieveShape n mp
                  assignIndex (id1, id2, id3) = (n !! tt (localOffset shape [id1, id2, id3])) := (tt (array3d ! (id1, id2, id2)))
               in Scoped $ map assignIndex $ indices array3d
            _ -> Empty

-- |
evaluateCodeC :: IsExpression e => e -> ValMap -> IO (ExitCode, String)
evaluateCodeC e valMap = do
  let exp = asRawExpr e
  readProcessWithExitCode "mkdir" ["C"] ""
  fileName <- generate $ vectorOf 10 $ elements ['A' .. 'Z']
  let cFilePath = "C" </> fileName <.> "c"
      executablePath = "C" </> fileName
      program = singleExpressionCProgram valMap exp
      libs = ["-lm", "-lfftw3"]
  TIO.writeFile cFilePath (T.intercalate "\n" program)
  readProcess "gcc" ([cFilePath, "-o", executablePath] ++ libs) ""
  (exitCode, output, _) <- readProcessWithExitCode executablePath [] ""
  readProcess "rm" [cFilePath] ""
  readProcess "rm" [executablePath] ""
  return (exitCode, output)

readMany :: Read a => String -> [a]
readMany = map read . filter (not . null) . splitOn " "

-- | Parse output of the C program
readAsInterpVal :: String -> InterpValue
readAsInterpVal str = case lines of
  [shapeStr, xs] ->
    case readMany shapeStr of
      [] -> VR . head . readMany $ xs
      [size] -> V1DR . listArray (0, size - 1) . readMany $ xs
      [size1, size2] -> V2DR . listArray ((0, 0), (size1 - 1, size2 - 1)) . readMany $ xs
  [shapeStr, rs, is] ->
    case readMany shapeStr of
      [] -> VC $ (head . readMany $ rs) +: (head . readMany $ is)
      [size] -> V1DC $ (listArray (0, size - 1) . readMany $ rs) +: (listArray (0, size - 1) . readMany $ is)
      [size1, size2] ->
        V2DC $ (listArray ((0, 0), (size1 - 1, size2 - 1)) . readMany $ rs) +: (listArray ((0, 0), (size1 - 1, size2 - 1)) . readMany $ is)
  where
    lines = splitOn "\n" str

prop_CEqualInterp :: XSuite -> Expectation
prop_CEqualInterp (XSuite exp valMap) = do
  (_, outputC) <- evaluateCodeC exp valMap
  let resultC = readAsInterpVal outputC
  let resultInterp = eval valMap exp
  resultC `shouldApprox` resultInterp

-- | Spec
spec :: Spec
spec =
  describe "C simple spec" $ do
    specify "C equal interp" $
      property prop_CEqualInterp
