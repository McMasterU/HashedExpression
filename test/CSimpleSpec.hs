{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module CSimpleSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Monad (replicateM_, unless, when)
import Data.Array
import Data.Complex (Complex (..))
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort, tails)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (ExitCode (..))
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Embed
import HashedExpression.Internal
import HashedExpression.Internal.Base hiding ((<.>))
import HashedExpression.Modeling.Typed
import HashedExpression.Internal.Node
import HashedExpression.Interp
import HashedExpression.Utils
import HashedExpression.Value
import System.FilePath
import System.Process (readProcess, readProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck
import Var
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
singleExpressionCProgram ::
  (Dimension d) => ValMap -> Expression d et -> Code
singleExpressionCProgram valMap expr =
  [ "#include <math.h>", --
    "#include <stdio.h>",
    "#include <stdlib.h>",
    "#include <complex.h>",
    if containsFTNode $ exMap expr
      then T.pack $ fftUtils
      else "",
    "int main()" --
  ]
    ++ scoped
      ( initMemory
          ++ fromCCode assigningValues
          ++ codes
          ++ fromCCode printValue
          ++ fromCCode releaseMemory
      )
  where
    (mp, n) = asExpression expr
    bound = product (retrieveShape n mp)
    et = retrieveElementType n mp
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
        for i bound [Printf ["%f ", n !! i]]
      | et == C =
        Scoped
          [ for i bound [Printf ["%f ", fun "creal" [n !! i]]],
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

hasFFTW :: IO Bool
hasFFTW = do
  fileName <- generate $ vectorOf 10 $ elements ['A' .. 'Z']
  let cFilePath = "C" </> fileName <.> "c"
      executablePath = "C" </> fileName
      codes = ["#include <fftw3.h>", "int main() {}"]
      program = T.intercalate "\n" . map T.pack $ codes
  TIO.writeFile cFilePath program
  (exitCode, output, _) <- readProcessWithExitCode "gcc" [cFilePath, "-o", executablePath, "-lm", "-lfftw3"] ""
  readProcessWithExitCode "rm" [cFilePath] ""
  readProcessWithExitCode "rm" [executablePath] ""
  return $ exitCode == ExitSuccess

-- |
evaluateCodeC :: (Dimension d) => Bool -> Expression d et -> ValMap -> IO (ExitCode, String)
evaluateCodeC withFT exp valMap = do
  readProcessWithExitCode "mkdir" ["C"] ""
  fileName <- generate $ vectorOf 10 $ elements ['A' .. 'Z']
  let cFilePath = "C" </> fileName <.> "c"
      executablePath = "C" </> fileName
      program = singleExpressionCProgram valMap exp
      libs
        | withFT = ["-lm", "-lfftw3"]
        | otherwise = ["-lm"]
  TIO.writeFile cFilePath (T.intercalate "\n" program)
  readProcess "gcc" ([cFilePath, "-o", executablePath] ++ libs) ""
  (exitCode, output, _) <- readProcessWithExitCode executablePath [] ""
  readProcess "rm" [cFilePath] ""
  readProcess "rm" [executablePath] ""
  return (exitCode, output)

-- | Parse output of the C program
readR :: String -> [Double]
readR = map read . filter (not . null) . splitOn " "

-- |
readC :: String -> ([Double], [Double])
readC str = (readR rePart, readR imPart)
  where
    [rePart, imPart] = splitOn "\n" str

-- |
prop_CEqualInterpScalarR :: SuiteScalarR -> Expectation
prop_CEqualInterpScalarR (Suite exp valMap) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputSimple) <- evaluateCodeC withFT exp valMap
      let resultNormalize = read . head . splitOn " " $ outputSimple
      let VR resultInterpNormalize = eval valMap exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpScalarC :: SuiteScalarC -> Expectation
prop_CEqualInterpScalarC (Suite exp valMap) = do
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputCodeC) <- evaluateCodeC withFT exp valMap
      let ([im], [re]) = readC outputCodeC
      let resultNormalize = im :+ re
      let VC resultInterpNormalize = eval valMap exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpOneR :: SuiteOneR -> Expectation
prop_CEqualInterpOneR (Suite exp valMap) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputSimple) <- evaluateCodeC withFT exp valMap
      let resultNormalize = listArray (0, defaultDim1D - 1) $ readR outputSimple
      let V1DR resultInterpNormalize = eval valMap exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpOneC :: SuiteOneC -> Expectation
prop_CEqualInterpOneC (Suite exp valMap) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputCodeC) <- evaluateCodeC withFT exp valMap
      let (re, im) = readC outputCodeC
          resultNormalize = listArray (0, defaultDim1D - 1) $ zipWith (:+) re im
          V1DC resultInterpNormalize = eval valMap exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpTwoR :: SuiteTwoR -> Expectation
prop_CEqualInterpTwoR (Suite exp valMap) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputSimple) <- evaluateCodeC withFT exp valMap
      let resultNormalize = listArray ((0, 0), (default1stDim2D - 1, default2ndDim2D - 1)) $ readR outputSimple
      let V2DR resultInterpNormalize = eval valMap exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpTwoC :: SuiteTwoC -> Expectation
prop_CEqualInterpTwoC (Suite exp valMap) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputCodeC) <- evaluateCodeC withFT exp valMap
      let (re, im) = readC outputCodeC
      let resultNormalize = listArray ((0, 0), (default1stDim2D - 1, default2ndDim2D - 1)) $ zipWith (:+) re im
      let V2DC resultInterpNormalize = eval valMap exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- | Spec
spec :: Spec
spec =
  describe "C simple spec" $ do
    specify "Evaluate hash interp should equal to C code evaluation (Expression Scalar R)" $
      property prop_CEqualInterpScalarR
    specify "Evaluate hash interp should equal to C code evaluation (Expression Scalar C)" $
      property prop_CEqualInterpScalarC
    specify "Evaluate hash interp should equal to C code evaluation (Expression One R)" $
      property prop_CEqualInterpOneR
    specify "Evaluate hash interp should equal to C code evaluation (Expression One C)" $
      property prop_CEqualInterpOneC
    specify "Evaluate hash interp should equal to C code evaluation (Expression Two R)" $
      property prop_CEqualInterpTwoR
    specify "Evaluate hash interp should equal to C code evaluation (Expression Two C)" $
      property prop_CEqualInterpTwoC
