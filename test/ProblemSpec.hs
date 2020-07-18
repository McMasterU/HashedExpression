{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module ProblemSpec where

import Commons
import Control.Applicative (liftA2)
import Control.Concurrent
import Control.Monad (forM_, replicateM, replicateM_, unless, when)
import Data.Array
import Data.Complex (Complex (..))
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Map.Strict (fromList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (traceShowId)
import GHC.IO.Exception (ExitCode (..))
import HashedExpression.Derivative
import HashedExpression.Internal
import HashedExpression.Internal.CollectDifferential
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize (normalize)
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation
import HashedExpression.Prettify (debugPrint, showExp, showExpDebug)
import HashedExpression.Problem
import HashedExpression.Value
import System.Process (readProcess, readProcessWithExitCode)
import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Var
import Prelude hiding ((^))
import qualified Prelude

-- |
prop_constructProblemNoConstraint :: SuiteScalarR -> Expectation
prop_constructProblemNoConstraint (Suite exp valMap) = do
  let names = Map.keys valMap
  let pdMap = partialDerivativeMaps . collectDifferentials . exteriorDerivative (Set.fromList names) $ exp
      constructResult = constructProblem exp names (Constraint [])
  case constructResult of
    NoVariables -> return () -- it is possible that the random expression doesn't have any variables
    ProblemInvalid reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    ProblemValid Problem {..} -> do
      let vars = map varName variables
      vars `shouldBe` Map.keys pdMap -- vars should be keys of partial differential map
      let ok variable
            | Just pId <- Map.lookup (varName variable) pdMap,
              pId == partialDerivativeId variable =
              True
            | otherwise = False
      assertBool "partial derivative ids aren't correct" $ all ok variables

-- |
makeValidBoxConstraint :: (String, Shape) -> IO ConstraintStatement
makeValidBoxConstraint (name, shape) =
  case shape of
    [] -> do
      let x = variable name
      val1 <- VScalar <$> generate arbitrary
      val2 <- VScalar <$> generate arbitrary
      generate $
        elements [x .<= val1, x .>= val2, x `between` (val1, val2)]
    [size] -> do
      let x = variable1D @Default1D name
      val1 <- V1D . listArray (0, size - 1) <$> generate (vectorOf size arbitrary)
      val2 <- V1D . listArray (0, size - 1) <$> generate (vectorOf size arbitrary)
      generate $ elements [x .<= val1, x .>= val2, x `between` (val1, val2)]
    [size1, size2] -> do
      let x = variable2D @Default2D1 @Default2D2 name
      val1 <- V2D . listArray ((0, 0), (size1 - 1, size2 - 1)) <$> generate (vectorOf (size1 * size2) arbitrary)
      val2 <- V2D . listArray ((0, 0), (size1 - 1, size2 - 1)) <$> generate (vectorOf (size1 * size2) arbitrary)
      generate $ elements [x .<= val1, x .>= val2, x `between` (val1, val2)]

--        [size1, size2, size3] -> do TODO -- add 3D for tests
--            val1 <-
--                V3D . listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) <$>
--                generate (vectorOf (size1 * size2 * size3) arbitrary)
--            val2 <-
--                V3D . listArray ((0, 0, 0), (size1 - 1, size2 - 1, size3 - 1)) <$>
--                generate (vectorOf (size1 * size2 * size3) arbitrary)
--            generate $
--                elements [x .<= val1, x .>= val2, x `between` (val1, val2)]

-- |
prop_constructProblemBoxConstraint :: SuiteScalarR -> Expectation
prop_constructProblemBoxConstraint (Suite exp valMap) = do
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
            | Just pId <- Map.lookup (varName variable) pdMap,
              pId == partialDerivativeId variable =
              True
            | otherwise = False
      assertBool "partial derivative ids aren't correct" $
        all ok variables
      case (sampled, boxConstraints) of
        (_ : _, []) ->
          assertFailure
            "Valid box constraints but not appear in the problem"
        (_, bs) -> length sampled `shouldBe` length bs

-- |
makeValidScalarConstraint :: IO ConstraintStatement
makeValidScalarConstraint = do
  sc <-
    fst <$> generate (sized (genScalarR @Default1D @Default2D1 @Default2D2))
  val1 <- VScalar <$> generate arbitrary
  val2 <- VScalar <$> generate arbitrary
  generate $ elements [sc .<= val1, sc .>= val2, sc `between` (val1, val2)]

-- |
prop_constructProblemScalarConstraints :: SuiteScalarR -> Expectation
prop_constructProblemScalarConstraints (Suite exp valMap) = do
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
            | Just pId <- Map.lookup (varName variable) pdMap,
              pId == partialDerivativeId variable =
              True
            | otherwise = False
      assertBool "partial derivative ids aren't correct" $
        all ok variables
      case (scc, scalarConstraints) of
        ([], _) -> return ()
        (_ : _, []) ->
          assertFailure
            "Having scalar constraints but not present in problem"
        (_, sConstraints) -> do
          let isOk sc =
                length (constraintPartialDerivatives sc)
                  `shouldBe` length vars
          assertBool "Empty constraint ?" $ not (null sConstraints)
          mapM_ isOk sConstraints

-- | List of hand-written problems and the expected result whether this problem is valid
problemsRepo :: [(ProblemResult, Bool)]
problemsRepo =
  [ ( let [x, y, z, t] = map (variable2D @128 @128) ["x", "y", "z", "t"]
          f = x <.> y + z <.> t
          constraints =
            Constraint
              [ x .>= VFile (TXT "x_lb.txt"),
                y .<= VFile (TXT "y_ub.txt"),
                x <.> z .>= VScalar 3
              ]
          vars = ["x", "y", "z", "t"]
       in constructProblem f vars constraints,
      True
    ),
    ( let [x, y, z, t] = map (variable2D @128 @128) ["x", "y", "z", "t"]
          f = x <.> y + z <.> t
          constraints =
            Constraint
              [ x .>= VScalar 5,
                y .<= VFile (TXT "y_ub.txt"),
                x <.> z .>= VScalar 3
              ]
          vars = ["x", "y", "z", "t"]
       in constructProblem f vars constraints,
      False
    ),
    ( let [x, y, z, t] = map (variable2D @128 @128) ["x", "y", "z", "t"]
          f = x <.> y + z <.> t
          constraints =
            Constraint [x .>= VNum 5, y .<= VNum 10, x <.> z .>= VNum 18]
          vars = ["x", "y", "z", "t"]
       in constructProblem f vars constraints,
      True
    ),
    ( let [x, y, z] = map (variable2D @100 @100) ["x", "y", "z"]
          f = x <.> y + z <.> z
          constraints = Constraint [y <.> z .<= VNum 1]
          vars = ["x", "y", "z"]
       in constructProblem f vars constraints,
      True
    )
  ]

spec :: Spec
spec =
  describe "Hash Solver spec " $ do
    specify "test hand-written problems" $
      forM_ problemsRepo $ \(problemResult, expected) -> do
        case (problemResult, expected) of
          (ProblemValid _, True) ->
            return ()
          (ProblemInvalid _, False) ->
            return ()
          _ -> assertFailure $ "Should be " ++ show expected ++ " to construct but result is " ++ show problemResult
--    specify "valid problem should be constructed successfully" $
--      property prop_constructProblemNoConstraint
--    specify "valid box constrained problem should be constructed successfully" $
--      property prop_constructProblemBoxConstraint
--    specify "valid scalar constraints problem should be successfully successfully" $
--      property prop_constructProblemScalarConstraints
