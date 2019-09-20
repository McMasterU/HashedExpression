{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module HashedToCSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Monad (replicateM_, unless)
import Data.Array
import Data.Complex (Complex(..))
import qualified Data.IntMap.Strict as IM
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
import HashedExpression
    ( C
    , DimensionType
    , Expression(..)
    , Node(..)
    , NumType
    , R
    , Scalar
    )
import HashedInner
import HashedInterp
import HashedNode
import HashedNormalize (normalize)
import HashedPrettify (showExp, showExpDebug)
import HashedToC
import HashedUtils
import System.Process (readProcess, readProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck

-- | Since we haven't generate C code for fourierTransform, we need this temporarily, otherwise tests will fail
--
containsFT :: (DimensionType d, NumType et) => Expression d et -> Bool
containsFT (Expression n mp) = any isFT $ IM.elems mp
  where
    isFT (_, node) =
        case node of
            ReFT _ -> True
            ImFT _ -> True
            _ -> False

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
prop_CEqualInterpScalarR (SuiteScalarR exp valMaps) =
    unless (containsFT exp) $ do
        (exitCode, outputSimple) <- evaluateCodeC (normalize exp) valMaps
        let resultNormalize = read . head . splitOn " " $ outputSimple
        let resultInterpNormalize = eval valMaps (normalize exp)
        resultNormalize `shouldApprox` resultInterpNormalize

-- |
--
prop_CEqualInterpScalarC :: SuiteScalarC -> Expectation
prop_CEqualInterpScalarC (SuiteScalarC exp valMaps) =
    unless (containsFT exp) $ do
        let normalizedExp = normalize exp
        writeFile "C/main.c" $
            intercalate "\n" . singleExpressionCProgram valMaps $ normalizedExp
        (exitCode, outputCodeC) <- evaluateCodeC (normalize exp) valMaps
        let ([im], [re]) = readC outputCodeC
        let resultNormalize = im :+ re
        let resultInterpNormalize = eval valMaps normalizedExp
        resultNormalize `shouldApprox` resultInterpNormalize

-- |
--
prop_CEqualInterpOneR :: SuiteOneR -> Expectation
prop_CEqualInterpOneR (SuiteOneR exp valMaps) =
    unless (containsFT exp) $ do
        (exitCode, outputSimple) <- evaluateCodeC (normalize exp) valMaps
        let resultNormalize = listArray (0, vectorSize - 1) $ readR outputSimple
        let resultInterpNormalize = eval valMaps (normalize exp)
        resultNormalize `shouldApprox` resultInterpNormalize

-- |
--
prop_CEqualInterpOneC :: SuiteOneC -> Expectation
prop_CEqualInterpOneC (SuiteOneC exp valMaps) =
    unless (containsFT exp) $ do
        let normalizedExp = normalize exp
        writeFile "C/main.c" $
            intercalate "\n" . singleExpressionCProgram valMaps $ normalizedExp
        (exitCode, outputCodeC) <- evaluateCodeC (normalize exp) valMaps
        let (re, im) = readC outputCodeC
        let resultNormalize = listArray (0, vectorSize - 1) $ zipWith (:+) re im
        let resultInterpNormalize = eval valMaps normalizedExp
        resultNormalize `shouldApprox` resultInterpNormalize

-- |
--
prop_CEqualInterpTwoR :: SuiteTwoR -> Expectation
prop_CEqualInterpTwoR (SuiteTwoR exp valMaps) = do
    unless (containsFT exp) $ do
        (exitCode, outputSimple) <- evaluateCodeC (normalize exp) valMaps
        let resultNormalize =
                listArray ((0, 0), (vectorSize - 1, vectorSize - 1)) $
                readR outputSimple
        let resultInterpNormalize = eval valMaps (normalize exp)
        resultNormalize `shouldApprox` resultInterpNormalize

-- |
--
prop_CEqualInterpTwoC :: SuiteTwoC -> Expectation
prop_CEqualInterpTwoC (SuiteTwoC exp valMaps) = do
    unless (containsFT exp) $ do
        let normalizedExp = normalize exp
        writeFile "C/main.c" $
            intercalate "\n" . singleExpressionCProgram valMaps $ normalizedExp
        (exitCode, outputCodeC) <- evaluateCodeC (normalize exp) valMaps
        let (re, im) = readC outputCodeC
        let resultNormalize =
                listArray ((0, 0), (vectorSize - 1, vectorSize - 1)) $
                zipWith (:+) re im
        let resultInterpNormalize = eval valMaps normalizedExp
        resultNormalize `shouldApprox` resultInterpNormalize

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
