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
import qualified Data.String.Interpolate as I
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Exception (ExitCode (..))
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Embed
import HashedExpression.Internal
import HashedExpression.Internal.Expression hiding ((<.>))
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Prettify
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

for1 :: T.Text -> Int -> Code -> Code
for1 iter bound codes =
  scoped $
    [ [I.i|int #{iter};|],
      [I.i|for (#{iter} = 0; #{iter} < #{bound}; #{iter}++)|]
    ]
      ++ scoped codes

-- | Generate a fully working C program that compute the expression and
-- print out the result, mostly used for testing
singleExpressionCProgram ::
  (Dimension d, NumType et) => ValMaps -> Expression d et -> Code
singleExpressionCProgram valMaps expr =
  [ "#include <math.h>", --
    "#include <stdio.h>",
    "#include <stdlib.h>",
    if containsFTNode $ exMap expr
      then T.pack $ fftUtils
      else "",
    "#include <complex.h>",
    "int main()" --
  ]
    ++ scoped (initMemory ++ assignVals ++ codes ++ printValue ++ releaseMemory)
  where
    (mp, n) = unwrap expr
    bound = product (retrieveShape n mp)
    et = retrieveElementType n mp
    codeGen = initCodegen CSimpleConfig {output = OutputText} mp []
    [i, j, k, nooffset] = ["i", "j", "k", "0"]
    initMemory = [[I.i|double *ptr = malloc(sizeof(double) * #{cMemSize codeGen});|]]
    -- assign value to variables
    assignVals = assigningValues codeGen valMaps
    -- codes to compute
    codes = evaluating codeGen [n]
    -- print the value of expression
    printValue
      | et == R = for1 i bound [[I.i|printf("%f ", #{(!!) codeGen n i});|]]
      | et == C =
        for1 i bound [[I.i|printf("%f ", #{(!!) codeGen n i});|]]
          ++ [[I.i|printf("\\n");|]]
          ++ for1 i bound [[I.i|printf("%f ", #{imAt codeGen n i});|]]
    releaseMemory = ["free(ptr);"]
    -------------------------------------------------------------------------------
    assigningValues :: CSimpleCodegen -> ValMaps -> Code
    assigningValues CSimpleCodegen {..} valMaps = concatMap assignValue names
      where
        [i, j, k, nooffset] = ["i", "j", "k", "0"]
        names :: [(Int, String)]
        names =
          let toVar nId
                | Var varName <- retrieveOp nId mp = Just (nId, varName)
                | Param name <- retrieveOp nId mp = Just (nId, name)
                | otherwise = Nothing
           in mapMaybe toVar . IM.keys $ mp
        assignValue :: (Int, String) -> Code
        assignValue (n, name) =
          case Map.lookup name valMaps of
            Just (VScalar val) -> [[I.i|#{n !! nooffset} = #{val};|]]
            Just (V1D array1d) ->
              let assignIndex id = [[I.i|#{n !! (showT id)} = #{array1d ! id};|]]
               in concatMap assignIndex $ indices array1d
            Just (V2D array2d) ->
              let shape = retrieveShape n mp
                  assignIndex (id1, id2) =
                    [[I.i|#{n !! showT (localOffset shape [id1, id2])} = #{array2d ! (id1, id2)};|]]
               in concatMap assignIndex $ indices array2d
            Just (V3D array3d) ->
              let shape = retrieveShape n mp
                  assignIndex (id1, id2, id3) =
                    [[I.i|#{n !! showT (localOffset shape [id1, id2, id3])} = #{array3d ! (id1, id2, id2)};|]]
               in concatMap assignIndex $ indices array3d
            _ -> []

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
evaluateCodeC :: (Dimension d, NumType et) => Bool -> Expression d et -> ValMaps -> IO (ExitCode, String)
evaluateCodeC withFT exp valMaps = do
  readProcessWithExitCode "mkdir" ["C"] ""
  fileName <- generate $ vectorOf 10 $ elements ['A' .. 'Z']
  let cFilePath = "C" </> fileName <.> "c"
      executablePath = "C" </> fileName
      program = singleExpressionCProgram valMaps exp
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
prop_CEqualInterpScalarR (Suite exp valMaps) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputSimple) <- evaluateCodeC withFT exp valMaps
      let resultNormalize = read . head . splitOn " " $ outputSimple
      let resultInterpNormalize = eval valMaps exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpScalarC :: SuiteScalarC -> Expectation
prop_CEqualInterpScalarC (Suite exp valMaps) = do
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputCodeC) <- evaluateCodeC withFT exp valMaps
      let ([im], [re]) = readC outputCodeC
      let resultNormalize = im :+ re
      let resultInterpNormalize = eval valMaps exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpOneR :: SuiteOneR -> Expectation
prop_CEqualInterpOneR (Suite exp valMaps) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputSimple) <- evaluateCodeC withFT exp valMaps
      let resultNormalize = listArray (0, defaultDim1D - 1) $ readR outputSimple
      let resultInterpNormalize = eval valMaps exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpOneC :: SuiteOneC -> Expectation
prop_CEqualInterpOneC (Suite exp valMaps) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputCodeC) <- evaluateCodeC withFT exp valMaps
      let (re, im) = readC outputCodeC
          resultNormalize = listArray (0, defaultDim1D - 1) $ zipWith (:+) re im
          resultInterpNormalize = eval valMaps exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpTwoR :: SuiteTwoR -> Expectation
prop_CEqualInterpTwoR (Suite exp valMaps) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputSimple) <- evaluateCodeC withFT exp valMaps
      let resultNormalize = listArray ((0, 0), (default1stDim2D - 1, default2ndDim2D - 1)) $ readR outputSimple
      let resultInterpNormalize = eval valMaps exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- |
prop_CEqualInterpTwoC :: SuiteTwoC -> Expectation
prop_CEqualInterpTwoC (Suite exp valMaps) =
  if containsFTNode $ exMap exp
    then do
      hasFTLib <- hasFFTW
      when hasFTLib $ proceed True
    else proceed False
  where
    proceed withFT = do
      (exitCode, outputCodeC) <- evaluateCodeC withFT exp valMaps
      let (re, im) = readC outputCodeC
      let resultNormalize = listArray ((0, 0), (default1stDim2D - 1, default2ndDim2D - 1)) $ zipWith (:+) re im
      let resultInterpNormalize = eval valMaps exp
      resultNormalize `shouldApprox` resultInterpNormalize

-- | Spec
spec :: Spec
spec =
  describe "C simple spec" $ do
    specify
      "Evaluate hash interp should equal to C code evaluation (Expression Scalar R)"
      $ property prop_CEqualInterpScalarR
    specify
      "Evaluate hash interp should equal to C code evaluation (Expression Scalar C)"
      $ property prop_CEqualInterpScalarC
    specify
      "Evaluate hash interp should equal to C code evaluation (Expression One R)"
      $ property prop_CEqualInterpOneR
    specify
      "Evaluate hash interp should equal to C code evaluation (Expression One C)"
      $ property prop_CEqualInterpOneC
    specify
      "Evaluate hash interp should equal to C code evaluation (Expression Two R)"
      $ property prop_CEqualInterpTwoR
    specify
      "Evaluate hash interp should equal to C code evaluation (Expression Two C)"
      $ property prop_CEqualInterpTwoC
