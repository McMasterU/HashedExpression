{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HashedSolverSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Monad (replicateM, replicateM_, unless, when)
import Data.Array
import Data.Complex (Complex(..))
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.UUID (toString)
import Data.UUID.V1 (nextUUID)
import Debug.Trace (traceShowId)
import GHC.IO.Exception (ExitCode(..))
import HashedCollect
import HashedDerivative
import HashedExpression
    ( C
    , DimensionType
    , Expression(..)
    , Node(..)
    , NumType
    , R
    , Scalar
    , Shape
    )
import HashedInner
import HashedInterp
import HashedNode
import HashedNormalize (normalize)
import HashedOperation (var, var1d, var2d, var3d)
import HashedPrettify (showExp, showExpDebug)
import HashedSolver
import HashedToC
import HashedUtils
import HashedVar
import System.Process (readProcess, readProcessWithExitCode)
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

-- |
--
isOneAfterAnother :: MemMap -> [Int] -> Bool
isOneAfterAnother memMap nIds = all isOk xs
  where
    xs = zip nIds (tail nIds)
    isOk (cur, nxt) =
        let (offsetCur, _, shapeCur) =
                fromJust $ IM.lookup cur (entryMap memMap)
            (offsetNxt, _, _) = fromJust $ IM.lookup nxt (entryMap memMap)
         in offsetCur + product shapeCur == offsetNxt

-- |
--
prop_constructProblemNoConstraint :: SuiteScalarR -> Expectation
prop_constructProblemNoConstraint (SuiteScalarR exp valMap) = do
    let names = Map.keys valMap
        pdMap =
            partialDerivativeMaps $
            collectDifferentials . exteriorDerivative (Set.fromList names) $ exp
        constructResult = constructProblem exp names NoConstraint
    case constructResult of
        NoVariables -> return () -- it is possible that the random expression doesn't have any variables
        ProblemInvalid reason ->
            assertFailure $ "Can't construct problem: " ++ reason
        ProblemValid Problem {..} -> do
            let vars = map varName variables
            vars `shouldBe` Map.keys pdMap -- vars should be keys of partial differential map
            let ok variable
                    | Just pId <- Map.lookup (varName variable) pdMap
                    , pId == partialDerivativeId variable = True
                    | otherwise = False
            assertBool "partial derivative ids aren't correct" $
                all ok variables
            assertBool "variables are not allocated consecutively" $
                isOneAfterAnother memMap (map nodeId variables)

-- |
--
makeValidBoxConstraint :: (String, Shape) -> IO ConstraintStatement
makeValidBoxConstraint (name, shape) =
    case shape of
        [] -> do
            let x = var name
            val1 <- VScalar <$> generate arbitrary
            val2 <- VScalar <$> generate arbitrary
            generate $
                elements [x .<= val1, x .>= val2, x `between` (val1, val2)]
        [size] -> do
            let x = var1d size name
            val1 <-
                V1D . listArray (0, size - 1) <$>
                generate (vectorOf size arbitrary)
            val2 <-
                V1D . listArray (0, size - 1) <$>
                generate (vectorOf size arbitrary)
            generate $
                elements [x .<= val1, x .>= val2, x `between` (val1, val2)]
        [size1, size2] -> do
            let x = var2d (size1, size2) name
            val1 <-
                V2D . listArray ((0, 0), (size1 - 1, size2 - 1)) <$>
                generate (vectorOf (size1 * size2) arbitrary)
            val2 <-
                V2D . listArray ((0, 0), (size1 - 1, size2 - 1)) <$>
                generate (vectorOf (size1 * size2) arbitrary)
            generate $
                elements [x .<= val1, x .>= val2, x `between` (val1, val2)]
        [size1, size2, size3] -> do
            let x = var3d (size1, size2, size3) name
            val1 <-
                V3D . listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) <$>
                generate (vectorOf (size1 * size2 * size3) arbitrary)
            val2 <-
                V3D . listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) <$>
                generate (vectorOf (size1 * size2 * size3) arbitrary)
            generate $
                elements [x .<= val1, x .>= val2, x `between` (val1, val2)]

-- |
--
prop_constructProblemBoxConstraint :: SuiteScalarR -> Expectation
prop_constructProblemBoxConstraint (SuiteScalarR exp valMap) = do
    let names = Map.keys valMap
    let df@(Expression dfN dfMp) =
            collectDifferentials . exteriorDerivative (Set.fromList names) $ exp
        pdMap = partialDerivativeMaps df
        vars = Map.keys pdMap
        varsWithShape = Map.toList $ Map.map (`retrieveShape` dfMp) pdMap
    bcs <- mapM makeValidBoxConstraint varsWithShape
    sampled <- generate $ sublistOf bcs
    let constraints = Constraint sampled
    let constructResult = constructProblem exp names constraints
    case constructResult of
        NoVariables -> return () -- it is possible that the random expression doesn't have any variables
        ProblemInvalid reason ->
            assertFailure $ "Can't construct problem: " ++ reason
        ProblemValid Problem {..} -> do
            let vars = map varName variables
            vars `shouldBe` Map.keys pdMap -- vars should be keys of partial differential map
            let ok variable
                    | Just pId <- Map.lookup (varName variable) pdMap
                    , pId == partialDerivativeId variable = True
                    | otherwise = False
            assertBool "partial derivative ids aren't correct" $
                all ok variables
            assertBool "variables are not allocated consecutively" $
                isOneAfterAnother memMap (map nodeId variables)
            case (sampled, boxConstraints) of
                (_:_, []) ->
                    assertFailure
                        "Valid box constraints but not appear in the problem"
                (_, bs) -> length sampled `shouldBe` length bs

-- |
--
makeValidScalarConstraint :: IO ConstraintStatement
makeValidScalarConstraint = do
    sc <- fst <$> generate genScalarR
    val1 <- VScalar <$> generate arbitrary
    val2 <- VScalar <$> generate arbitrary
    generate $ elements [sc .<= val1, sc .>= val2, sc `between` (val1, val2)]

-- |
--
prop_constructProblemScalarConstraints :: SuiteScalarR -> Expectation
prop_constructProblemScalarConstraints (SuiteScalarR exp valMap) = do
    let names = Map.keys valMap
    let df@(Expression dfN dfMp) =
            collectDifferentials . exteriorDerivative (Set.fromList names) $ exp
        pdMap = partialDerivativeMaps df
        vars = Map.keys pdMap
        varsWithShape = Map.toList $ Map.map (`retrieveShape` dfMp) pdMap
    -- box constraints
    bcs <- mapM makeValidBoxConstraint varsWithShape
    sampled <- generate $ sublistOf bcs
    -- scalar constraints
    numScalarConstraint <- generate $ elements [2 .. 4]
    scc <- replicateM numScalarConstraint makeValidScalarConstraint
    let constraints = Constraint $ sampled ++ scc
    let constructResult = constructProblem exp names constraints
    case constructResult of
        NoVariables -> return () -- it is possible that the random expression doesn't have any variables
        ProblemInvalid reason ->
            assertFailure $ "Can't construct problem: " ++ reason
        ProblemValid Problem {..} -> do
            let vars = map varName variables
            vars `shouldBe` Map.keys pdMap -- vars should be keys of partial differential map
            let ok variable
                    | Just pId <- Map.lookup (varName variable) pdMap
                    , pId == partialDerivativeId variable = True
                    | otherwise = False
            assertBool "partial derivative ids aren't correct" $
                all ok variables
            assertBool "variables are not allocated consecutively" $
                isOneAfterAnother memMap (map nodeId variables)
            case (scc, scalarConstraints) of
                ([], _) -> return ()
                (_:_, []) ->
                    assertFailure
                        "Having scalar constraints but not present in problem"
                (_, sConstraints) -> do
                    let isOk sc =
                            length (constraintPartialDerivatives sc) `shouldBe`
                            length vars
                    assertBool "Empty constraint ?" $ not (null sConstraints)
                    mapM_ isOk sConstraints

spec :: Spec
spec =
    describe "Hash Solver spec " $ do
        specify "valid problem should be constructed successfully" $
            property prop_constructProblemNoConstraint
        specify
            "valid box constrained problem should be constructed successfully" $
            property prop_constructProblemBoxConstraint
        specify
            "valid scalar constraints problem should be successfully successfully" $
            property prop_constructProblemScalarConstraints
