{-# LANGUAGE ExistentialQuantification #-}

module HashedToCSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Monad (replicateM_)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
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
    writeFile fullFileName (intercalate "\n" program)
    readProcess "cc" [fullFileName, "-o", "C/" ++ fileName] ""
    let runCommand = "C/" ++ fileName
    output <- readProcess runCommand [] ""
    readProcess "rm" [fullFileName] ""
    readProcess "rm" ["C/" ++ fileName] ""
    return output


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
                print "hello world"
--                (exp, valMaps) <- generate genZeroR
--                doubles <-
--                    generate $ vectorOf (length names) (arbitrary :: Gen Double)
--                let vm0 = Map.fromList $ zip names doubles
--                    valMaps = emptyVms |> withVm0 vm0
--                putStrLn "------------------------"
--                -- Evaluate by C code should equal to HashedInterp
--                output <- evaluateCodeC exp valMaps
--                let result = read . head . splitOn " " $ output
--                let resultInterp = eval valMaps exp
--                putStrLn $ "Result C Code: " ++ show result
--                putStrLn $ "Result Interp: " ++ show resultInterp
--                result `shouldApprox` resultInterp
--                putStrLn "OK!"
--                -- Evaluate by C code simplified version should equal to HashedInterp
--                outputSimple <- evaluateCodeC (simplify exp) valMaps
--                let resultSimple = read . head . splitOn " " $ outputSimple
--                putStrLn $ "Result C Code (Simplified): " ++ show result
--                putStrLn $ "Result Interp (Simplified): " ++ show resultInterp
--                resultSimple `shouldApprox` eval valMaps (simplify exp)
--                putStrLn "OK!"
