{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module SolverSpec where

import Commons
import Control.Monad (forM)
import Data.Array
import Data.List.Extra (trim)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import GHC.IO.Exception (ExitCode (..))
import HashedExpression.Codegen
import HashedExpression.Codegen.CSimple
import HashedExpression.Internal.Base
import HashedExpression.Interp
import HashedExpression.Modeling.Typed
import HashedExpression.Problem
import HashedExpression.Value
import System.FilePath hiding ((<.>))
import System.Process
import Test.Hspec
import Test.QuickCheck hiding (Right)
import Prelude hiding ((!!), (^))

runCommandIn :: FilePath -> String -> IO ExitCode
runCommandIn cwd cmd = do
  (_, _, _, ph) <-
    createProcess
      (shell cmd)
        { cwd = Just cwd,
          std_in = NoStream,
          std_out = NoStream,
          std_err = NoStream
        }
  waitForProcess ph

readValScalar :: String -> Double
readValScalar = read . trim

readVal1D :: Int -> String -> Array Int Double
readVal1D sz content = listArray (0, sz - 1) . map read . splitOn " " $ trim content

readVal2D :: (Int, Int) -> String -> Array (Int, Int) Double
readVal2D (sz1, sz2) content = listArray ((0, 0), (sz1 - 1, sz2 - 1)) . map read . splitOn " " $ trim content

getValueScalar :: String -> Map String String -> Double
getValueScalar name = readValScalar . fromJust . Map.lookup name

getValue1D :: String -> Int -> Map String String -> Array Int Double
getValue1D name size = readVal1D size . fromJust . Map.lookup name

getValue2D :: String -> (Int, Int) -> Map String String -> Array (Int, Int) Double
getValue2D name size = readVal2D size . fromJust . Map.lookup name

solveProblem :: Problem -> ValMap -> IO (Map String String)
solveProblem problem valMap = do
  case generateProblemCode CSimpleConfig {output = OutputText, maxIteration = Nothing} problem valMap of
    -- TODO: refine this
    Right proceed -> do
      folderName <- generate $ vectorOf 10 $ elements ['A' .. 'Z']
      let cwd = "C" </> folderName
      readProcess "cp" ["-R", "solvers/lbfgs-b", cwd] ""
      proceed cwd
      runCommandIn cwd "make"
      runCommandIn cwd "./lbfgs-b"
      result <- forM (variables problem) $ \v -> do
        let outputFile = varName v ++ "_out.txt"
        content <- readFile $ cwd </> outputFile
        return (varName v, content)
      readProcess "rm" ["-rf", cwd] ""
      return $ Map.fromList result

prop_Rosenbrock :: Double -> Double -> Property
prop_Rosenbrock a b =
  (a /= 0 && b > 0) ==> do
    let x = variable "x"
        y = variable "y"
    let obj = (constant a - x) ^ 2 + constant b * (y - x ^ 2) ^ 2
    case constructProblem obj (Constraint []) of
      Right p -> do
        res <- solveProblem p Map.empty
        let xGot = getValueScalar "x" res
        let yGot = getValueScalar "y" res
        xGot `shouldApprox` a
        yGot `shouldApprox` a ^ 2

-- | Spec
spec :: Spec
spec =
  describe "Optimization solver spec" $ do
    specify "Simple paraboloid" $ do
      let x = variable1D @10 "x"
      let obj = (x - 10) <.> (x - 10)
      case constructProblem obj (Constraint []) of
        Right p -> do
          res <- solveProblem p Map.empty
          let xGot = getValue1D "x" 10 res
          let xExpect = listArray (0, 9) $ replicate 10 10
          xGot `shouldApprox` xExpect
    specify "Entropy" $ do
      let x = variable2D @5 @5 "x"
      let obj = (x * log x) <.> 1
      case constructProblem obj (Constraint []) of
        Right p -> do
          res <- solveProblem p Map.empty
          let xGot = getValue2D "x" (5, 5) res
          let xExpect = listArray ((0, 0), (4, 4)) $ replicate 25 (1 / 2.7182818285)
          xGot `shouldApprox` xExpect
    specify "Fourier transform" $ do
      let x = variable1D @10 "x"
      let y = variable1D @10 "y"
      let a = param1D @10 "a"
      let b = param1D @10 "b"
      let obj = norm2square (ft (x +: y) - (a +: b))
      case constructProblem obj (Constraint []) of
        Right p -> do
          valA <- listArray (0, 9) <$> generate (vectorOf 10 arbitrary)
          valB <- listArray (0, 9) <$> generate (vectorOf 10 arbitrary)
          res <- solveProblem p (Map.fromList [("a", V1D valA), ("b", V1D valB)])
          let xGot = getValue1D "x" 10 res
          let yGot = getValue1D "y" 10 res
          let xExpect = xRe (fourierTransform1D FT_BACKWARD 10 (valA +: valB))
          let yExpect = xIm (fourierTransform1D FT_BACKWARD 10 (valA +: valB))
          xGot `shouldApprox` xExpect
          yGot `shouldApprox` yExpect
    specify "Rosenbrock, 2 variables different parameters" $ do
      property prop_Rosenbrock
    specify "Rosenbrock multidimensional" $ do
      let x = variable1D @20 "x"
      let evenX = project (ranges @0 @18 @2) x
      let oddX = project (ranges @1 @19 @2) x
      let obj = 100 * norm2square (evenX ^ 2 - oddX) + norm2square (evenX - 1)
      case constructProblem obj (Constraint []) of
        Right p -> do
          res <- solveProblem p Map.empty
          let xGot = getValue1D "x" 20 res
          let xExpect = listArray (0, 19) $ replicate 20 1
          xGot `shouldApprox` xExpect
