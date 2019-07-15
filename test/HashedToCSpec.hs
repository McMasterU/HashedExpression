{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module HashedToCSpec where

import Commons
import Control.Applicative (liftA2)
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
import HashedExpression (C, DimensionType, Expression(..), NumType, R, Zero)
import HashedInner
import HashedInterp
import HashedNode
import HashedPrettify (showExp)
import HashedSimplify (simplify)
import HashedToC
import HashedUtils
import System.Process (readProcess, readProcessWithExitCode)
import Test.Hspec
import Test.QuickCheck

-- |
--
data ArbitraryExpresion =
    forall d et. ArbitraryExpresion (Expression d et)

instance Show ArbitraryExpresion where
    show (ArbitraryExpresion exp) = show exp

instance Arbitrary ArbitraryExpresion where
    arbitrary =
        let option1 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Zero R))
            option2 =
                fmap ArbitraryExpresion (arbitrary :: Gen (Expression Zero C))
         in oneof [option1, option2]

-- | Property of topological sort
--
prop_TopologicalSort :: ArbitraryExpresion -> Bool
prop_TopologicalSort (ArbitraryExpresion (Expression n mp)) =
    noDuplicate && all prop withChildren
  where
    sortedNodeId = topologicalSort (mp, n)
    noDuplicate = sort (removeDuplicate sortedNodeId) == sort sortedNodeId
    isAfter n other =
        filter (liftA2 (||) (== n) (== other)) sortedNodeId == [other, n]
    dependencies n = nodeArgs $ retrieveNode n mp
    withChildren = zip sortedNodeId (map dependencies sortedNodeId)
    prop (nId, childrenIds) = all (nId `isAfter`) childrenIds

-- |
--
evaluateCodeC ::
       (DimensionType d, NumType et) => Expression d et -> ValMaps -> IO String
evaluateCodeC exp valMaps = do
    readProcessWithExitCode "mkdir" ["C"] ""
    fileName <- fmap (toString . fromJust) nextUUID
    let fullFileName = "C/" ++ fileName ++ ".c"
    let program = generateProgram valMaps exp
    TIO.writeFile fullFileName (T.intercalate "\n" . map T.pack $ program)
    readProcess "cc" [fullFileName, "-o", "C/" ++ fileName] ""
    let runCommand = "C/" ++ fileName
    output <- readProcess runCommand [] ""
    readProcess "rm" [fullFileName] ""
    readProcess "rm" ["C/" ++ fileName] ""
    return output

-- | Parse output of the C program
--
readR :: String -> [Double]
readR = map read . filter (not . null) . splitOn " "

readC :: String -> ([Double], [Double])
readC str = (readR rePart, readR imPart)
  where
    [rePart, imPart] = splitOn "\n" str

-- | Spec
--
spec :: Spec
spec =
    describe "Hashed To C spec" $ do
        specify "Topological sort" $ property prop_TopologicalSort
        specify "Compute local offset" $ do
            localOffset [2, 3, 4] [1, 2, 0] `shouldBe` (1 * 3 * 4 + 2 * 4 + 0)
            localOffset [5] [3] `shouldBe` 3
            localOffset [2, 3, 5, 6] [0, 0, 0, 0] `shouldBe` 0
        specify "Size memmap" $ pendingWith "not implemented"
        specify "Mem map offset" $ pendingWith "not implemented"
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression Zero R)" $
            replicateM_ 10 $ do
                SuiteZeroR exp valMaps <- generate arbitrary
                putStrLn "------------------------"
                -- Evaluate by C code should equal to HashedInterp
                output <- evaluateCodeC exp valMaps
                let result = read . head . splitOn " " $ output
                let resultInterp = eval valMaps exp
                putStrLn $ "Result C Code: " ++ show result
                putStrLn $ "Result Interp: " ++ show resultInterp
                result `shouldApprox` resultInterp
                putStrLn "OK!"
                -- Evaluate by C code simplified version should equal to HashedInterp
                outputSimple <- evaluateCodeC (simplify exp) valMaps
                let resultSimplify = read . head . splitOn " " $ outputSimple
                let resultInterpSimplify = eval valMaps (simplify exp)
                putStrLn $ "Result C Code (Simplified): " ++ show resultSimplify
                putStrLn $
                    "Result Interp (Simplified): " ++ show resultInterpSimplify
                resultSimplify `shouldApprox` resultInterpSimplify
                putStrLn "OK!"
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression Zero C)" $
            replicateM_ 10 $ do
                SuiteZeroC exp valMaps <- generate arbitrary
                putStrLn "------------------------"
                -- Evaluate by C code simplified version should equal to HashedInterp
                let simplifiedExp = simplify exp
                writeFile "C/main.c" $
                    intercalate "\n" . generateProgram valMaps $ simplifiedExp
                outputCodeC <- evaluateCodeC (simplify exp) valMaps
                let ([im], [re]) = readC outputCodeC
                let resultSimplify = im :+ re
                let resultInterpSimplify = eval valMaps simplifiedExp
                putStrLn $ "Result C Code (Simplified): " ++ show resultSimplify
                putStrLn $
                    "Result Interp (Simplified): " ++ show resultInterpSimplify
                resultSimplify `shouldApprox` resultInterpSimplify
                putStrLn "OK!"
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression One R)" $
            replicateM_ 10 $ do
                SuiteOneR exp valMaps <- generate arbitrary
                putStrLn "------------------------"
                -- Evaluate by C code should equal to HashedInterp
                output <- evaluateCodeC exp valMaps
                let result = listArray (0, vectorSize - 1) $ readR output
                let resultInterp = eval valMaps exp
                putStrLn $ "Result C Code: " ++ show result
                putStrLn $ "Result Interp: " ++ show resultInterp
                result `shouldApprox` resultInterp
                putStrLn "OK!"
                -- Evaluate by C code simplified version should equal to HashedInterp
                outputSimple <- evaluateCodeC (simplify exp) valMaps
                let resultSimplify =
                        listArray (0, vectorSize - 1) $ readR output
                let resultInterpSimplify = eval valMaps (simplify exp)
                putStrLn $ "Result C Code (Simplified): " ++ show resultSimplify
                putStrLn $
                    "Result Interp (Simplified): " ++ show resultInterpSimplify
                resultSimplify `shouldApprox` resultInterpSimplify
                putStrLn "OK!"
        specify
            "Evaluate hash interp should equal to C code evaluation (Expression One C)" $
            replicateM_ 1 $ do
                SuiteOneC exp valMaps <- generate arbitrary
                putStrLn "------------------------"
                -- Evaluate by C code simplified version should equal to HashedInterp
                let simplifiedExp = simplify exp
                writeFile "C/main.c" $
                    intercalate "\n" . generateProgram valMaps $ simplifiedExp
                outputCodeC <- evaluateCodeC (simplify exp) valMaps
                putStrLn outputCodeC
--                let (re, im) = readC outputCodeC
--                let resultSimplify = listArray (0, vectorSize) $ zipWith (:+) re im
--                let resultInterpSimplify = eval valMaps simplifiedExp
--                putStrLn $ "Result C Code (Simplified): " ++ show resultSimplify
--                putStrLn $
--                    "Result Interp (Simplified): " ++ show resultInterpSimplify
--                resultSimplify `shouldApprox` resultInterpSimplify
--                putStrLn "OK!"
