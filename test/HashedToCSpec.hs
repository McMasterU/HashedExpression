{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module HashedToCSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Monad (replicateM_)
import Data.Array
import Data.Complex (Complex(..))
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.UUID (toString)
import Data.UUID.V1 (nextUUID)
import Debug.Trace (traceShowId)
import GHC.IO.Exception (ExitCode(..))
import HashedExpression (C, DimensionType, Expression(..), NumType, R, Scalar)
import HashedInner
import HashedInterp
import HashedNode
import HashedPrettify (showExp, showExpDebug)
import HashedSimplify (simplify)
import HashedToC
import HashedUtils
import System.Process (readProcess, readProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck

-- |
--
evaluateCodeC ::
       (DimensionType d, NumType et)
    => Expression d et
    -> ValMaps
    -> IO (ExitCode, String)
evaluateCodeC exp valMaps = do
    readProcessWithExitCode "mkdir" ["C"] ""
    fileName <- fmap (toString . fromJust) nextUUID
    let fullFileName = "C/" ++ fileName ++ ".c"
    let program = singleExpressionCProgram valMaps exp
    TIO.writeFile fullFileName (T.intercalate "\n" . map T.pack $ program)
    readProcess "gcc" [fullFileName, "-o", "C/" ++ fileName, "-lm"] ""
    let runCommand = "C/" ++ fileName
    (exitCode, output, _) <- readProcessWithExitCode runCommand [] ""
    readProcess "rm" [fullFileName] ""
    readProcess "rm" ["C/" ++ fileName] ""
    return (exitCode, output)

-- | Parse output of the C program
--
readR :: String -> [Double]
readR = map read . filter (not . null) . splitOn " "

-- |
--
readC :: String -> ([Double], [Double])
readC str = (readR rePart, readR imPart)
  where
    [rePart, imPart] = splitOn "\n" str

-- |
--
prop_CEqualInterpScalarR :: SuiteScalarR -> Expectation
prop_CEqualInterpScalarR (SuiteScalarR exp valMaps) = do
    (exitCode, outputSimple) <- evaluateCodeC (simplify exp) valMaps
    let resultSimplify = read . head . splitOn " " $ outputSimple
    let resultInterpSimplify = eval valMaps (simplify exp)
    resultSimplify `shouldApprox` resultInterpSimplify

-- |
--
prop_CEqualInterpScalarC :: SuiteScalarC -> Expectation
prop_CEqualInterpScalarC (SuiteScalarC exp valMaps) = do
    let simplifiedExp = simplify exp
    writeFile "C/main.c" $
        intercalate "\n" . singleExpressionCProgram valMaps $ simplifiedExp
    (exitCode, outputCodeC) <- evaluateCodeC (simplify exp) valMaps
    let ([im], [re]) = readC outputCodeC
    let resultSimplify = im :+ re
    let resultInterpSimplify = eval valMaps simplifiedExp
    resultSimplify `shouldApprox` resultInterpSimplify

-- |
--
prop_CEqualInterpOneR :: SuiteOneR -> Expectation
prop_CEqualInterpOneR (SuiteOneR exp valMaps) = do
    (exitCode, outputSimple) <- evaluateCodeC (simplify exp) valMaps
    let resultSimplify = listArray (0, vectorSize - 1) $ readR outputSimple
    let resultInterpSimplify = eval valMaps (simplify exp)
    resultSimplify `shouldApprox` resultInterpSimplify

-- |
--
prop_CEqualInterpOneC :: SuiteOneC -> Expectation
prop_CEqualInterpOneC (SuiteOneC exp valMaps) = do
    let simplifiedExp = simplify exp
    writeFile "C/main.c" $
        intercalate "\n" . singleExpressionCProgram valMaps $ simplifiedExp
    (exitCode, outputCodeC) <- evaluateCodeC (simplify exp) valMaps
    let (re, im) = readC outputCodeC
    let resultSimplify = listArray (0, vectorSize - 1) $ zipWith (:+) re im
    let resultInterpSimplify = eval valMaps simplifiedExp
    resultSimplify `shouldApprox` resultInterpSimplify

-- |
--
prop_CEqualInterpTwoR :: SuiteTwoR -> Expectation
prop_CEqualInterpTwoR (SuiteTwoR exp valMaps) = do
    (exitCode, outputSimple) <- evaluateCodeC (simplify exp) valMaps
    let resultSimplify =
            listArray ((0, 0), (vectorSize - 1, vectorSize - 1)) $
            readR outputSimple
    let resultInterpSimplify = eval valMaps (simplify exp)
    resultSimplify `shouldApprox` resultInterpSimplify

-- |
--
prop_CEqualInterpTwoC :: SuiteTwoC -> Expectation
prop_CEqualInterpTwoC (SuiteTwoC exp valMaps) = do
    let simplifiedExp = simplify exp
    writeFile "C/main.c" $
        intercalate "\n" . singleExpressionCProgram valMaps $ simplifiedExp
    (exitCode, outputCodeC) <- evaluateCodeC (simplify exp) valMaps
    let (re, im) = readC outputCodeC
    let resultSimplify =
            listArray ((0, 0), (vectorSize - 1, vectorSize - 1)) $
            zipWith (:+) re im
    let resultInterpSimplify = eval valMaps simplifiedExp
    resultSimplify `shouldApprox` resultInterpSimplify

-- | Spec
--
spec :: Spec
spec =
    describe "Hashed To C spec" $ do
        specify "Compute local offset" $ do
            localOffset [2, 3, 4] [1, 2, 0] `shouldBe` (1 * 3 * 4 + 2 * 4 + 0)
            localOffset [5] [3] `shouldBe` 3
            localOffset [2, 3, 5, 6] [0, 0, 0, 0] `shouldBe` 0
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression Scalar R)" $
            property prop_CEqualInterpScalarR
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression Scalar C)" $
            property prop_CEqualInterpScalarC
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression One R)" $
            property prop_CEqualInterpOneR
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression One C)" $
            property prop_CEqualInterpOneC
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression Two R)" $
            property prop_CEqualInterpTwoR
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression Two C)" $
            property prop_CEqualInterpTwoC
