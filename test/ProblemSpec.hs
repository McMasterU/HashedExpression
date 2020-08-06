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
import HashedExpression.Differentiation.Exterior.Collect
import HashedExpression.Differentiation.Exterior.Derivative
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Operation
import HashedExpression.Prettify
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
  let constructResult = constructProblem exp (Constraint [])
  case constructResult of
    ProblemInvalid reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    ProblemValid Problem {..} -> do
      return ()

--      let vars = map varName variables
--      vars `shouldBe` Map.keys pdMap -- vars should be keys of partial differential map
--      let ok variable
--            | Just pId <- Map.lookup (varName variable) pdMap,
--              pId == partialDerivativeId variable =
--              True
--            | otherwise = False
--      assertBool "partial derivative ids aren't correct" $ all ok variables

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
  let varsWithShape = varNodesWithShape (exMap exp)
  bcs <- mapM makeValidBoxConstraint varsWithShape
  sampled <- generate $ sublistOf bcs
  let constraints = Constraint sampled
  let constructResult = constructProblem exp constraints
  case constructResult of
    ProblemInvalid reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    ProblemValid Problem {..} -> do
      case (sampled, boxConstraints) of
        (_ : _, []) ->
          assertFailure
            "Valid box constraints but not appear in the problem"
        (_, bs) -> return ()

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
  --  print "-------------------------------------------------------------"
  --  showExp $ exp
  let names = Map.keys valMap
  --  print names
  let varsWithShape = varNodesWithShape (exMap exp)
  --  print $ varsWithShape
  -- box constraints
  bcs <- mapM makeValidBoxConstraint varsWithShape
  sampled <- generate $ sublistOf bcs
  -- scalar constraints
  numScalarConstraint <- generate $ elements [2 .. 4]
  scc <- replicateM numScalarConstraint makeValidScalarConstraint
  --  forM_ scc $ \sc -> print $ debugPrint $ getExpressionCS sc
  let constraints = Constraint $ sampled ++ scc
  let constructResult = constructProblem exp constraints
  case constructResult of
    ProblemInvalid reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    ProblemValid Problem {..} -> do
      case (scc, scalarConstraints) of
        ([], _) -> return ()
        (_ : _, []) ->
          assertFailure
            "Having scalar constraints but not present in problem"
        (_, sConstraints) -> do
          let isOk sc = length (constraintPartialDerivatives sc) `shouldBe` length variables
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
       in constructProblem f constraints,
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
       in constructProblem f constraints,
      False
    ),
    ( let [x, y, z, t] = map (variable2D @128 @128) ["x", "y", "z", "t"]
          f = x <.> y + z <.> t
          constraints =
            Constraint [x .>= VNum 5, y .<= VNum 10, x <.> z .>= VNum 18]
          vars = ["x", "y", "z", "t"]
       in constructProblem f constraints,
      True
    ),
    ( let [x, y, z] = map (variable2D @100 @100) ["x", "y", "z"]
          f = x <.> y + z <.> z
          constraints = Constraint [y <.> z .<= VNum 1]
          vars = ["x", "y", "z"]
       in constructProblem f constraints,
      True
    )
  ]

printVariables :: Problem -> [String]
printVariables Problem {..} =
  map (\Variable {..} -> debugPrint (expressionMap, nodeId)) variables

printPartialDerivatives :: Problem -> [String]
printPartialDerivatives Problem {..} =
  map (\Variable {..} -> debugPrint (expressionMap, partialDerivativeId)) variables

printScalarConstraintPartialDerivatives :: Problem -> ScalarConstraint -> [String]
printScalarConstraintPartialDerivatives Problem {..} ScalarConstraint {..} =
  map (\id -> debugPrint (expressionMap, id)) constraintPartialDerivatives

printScalarConstraintsPartialDerivatives :: Problem -> [[String]]
printScalarConstraintsPartialDerivatives problem@Problem {..} = map (printScalarConstraintPartialDerivatives problem) scalarConstraints

spec :: Spec
spec =
  describe "Hash Solver spec " $ do
    --    specify "unit test 1" $ do
    --      let obj = x1 <.> y1 + x + y
    --          constraints = Constraint []
    --      case constructProblem obj constraints of
    --        ProblemInvalid _ -> assertFailure "should construct problem properly"
    --        ProblemValid problem -> do
    --          printVariables problem `shouldBe` [debugPrintExp x, debugPrintExp y, debugPrintExp x1, debugPrintExp y1]
    --          printPartialDerivatives problem `shouldBe` [debugPrintExp (constant 1), debugPrintExp (constant 1), debugPrintExp y1, debugPrintExp x1]
    --    specify "unit test 2" $ do
    --      let obj = x2 <.> y2
    --          constraints = Constraint [z + y .>= VNum 1, z + x .<= VNum 2]
    --      case constructProblem obj constraints of
    --        ProblemInvalid _ -> assertFailure "should construct problem properly"
    --        ProblemValid problem -> do
    --          printVariables problem `shouldBe` [debugPrintExp x, debugPrintExp y, debugPrintExp z, debugPrintExp x2]
    --          printPartialDerivatives problem `shouldBe` [debugPrintExp (constant 0), debugPrintExp (constant 0), debugPrintExp (constant 0), debugPrintExp y2]
    --          printScalarConstraintsPartialDerivatives problem
    --            `shouldBe` [ [debugPrintExp (constant 1), debugPrintExp (constant 0), debugPrintExp (constant 1), debugPrintExp zero2],
    --                         [debugPrintExp (constant 0), debugPrintExp (constant 1), debugPrintExp (constant 1), debugPrintExp zero2]
    --                       ]
    --    specify "unit test 3" $ do
    --      let obj = 1
    --          constraints = Constraint [x2 <.> a2 .>= VNum 1, m + 2 * n .<= VNum 2]
    --      case constructProblem obj constraints of
    --        ProblemInvalid _ -> assertFailure "should construct problem properly"
    --        ProblemValid problem -> do
    --          printVariables problem `shouldBe` [debugPrintExp m, debugPrintExp n, debugPrintExp x2]
    --          printPartialDerivatives problem `shouldBe` [debugPrintExp (constant 0), debugPrintExp (constant 0), debugPrintExp zero2]
    --          printScalarConstraintsPartialDerivatives problem
    --            `shouldBe` [ [debugPrintExp (constant 1), debugPrintExp (constant 2), debugPrintExp zero2],
    --                         [debugPrintExp (constant 0), debugPrintExp (constant 0), debugPrintExp a2]
    --                       ]
    specify "test hand-written problems" $
      forM_ problemsRepo $ \(problemResult, expected) -> do
        case (problemResult, expected) of
          (ProblemValid p, True) -> do
            print p
            return ()
          (ProblemInvalid _, False) ->
            return ()
          _ -> assertFailure $ "Should be " ++ show expected ++ " to construct but result is " ++ show problemResult
    specify "valid problem should be constructed successfully" $
      property prop_constructProblemNoConstraint
    specify "valid box constrained problem should be constructed successfully" $
      property prop_constructProblemBoxConstraint
    specify "valid scalar constraints problem should be successfully successfully" $
      property prop_constructProblemScalarConstraints
