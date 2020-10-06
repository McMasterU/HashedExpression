{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module ProblemSpec where

import Commons
import Control.Monad (replicateM)
import Data.Array
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Modeling.Typed
import HashedExpression.Problem
import HashedExpression.Value
import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Prelude hiding ((^))

-- |
prop_constructProblemNoConstraint :: Expression Scalar R -> Expectation
prop_constructProblemNoConstraint exp = do
  let constructResult = constructProblem exp (Constraint [])
  case constructResult of
    Left reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    Right Problem {..} -> do
      return ()

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
      let x = fromNodeUnwrapped (shape, R, Var name)
      val1 <- V1D . listArray (0, size - 1) <$> generate (vectorOf size arbitrary)
      val2 <- V1D . listArray (0, size - 1) <$> generate (vectorOf size arbitrary)
      generate $ elements [x .<= val1, x .>= val2, x `between` (val1, val2)]
    [size1, size2] -> do
      let x = fromNodeUnwrapped (shape, R, Var name)
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

varNodesWithShape :: ExpressionMap -> [(String, Shape)]
varNodesWithShape mp = map (\(name, shape, _) -> (name, shape)) $ varNodes mp

-- |
prop_constructProblemBoxConstraint :: Expression Scalar R -> Expectation
prop_constructProblemBoxConstraint e = do
  let exp = asRawExpr e
  let vs = varNodesWithShape $ fst exp
  bcs <- mapM makeValidBoxConstraint vs
  sampled <- generate $ sublistOf bcs
  let constraints = Constraint sampled
  let constructResult = constructProblem exp constraints
  case constructResult of
    Left reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    Right Problem {..} -> do
      case (sampled, boxConstraints) of
        (_ : _, []) ->
          assertFailure
            "Valid box constraints but not appear in the problem"
        (_, bs) -> return ()

-- |
makeValidScalarConstraint :: IO ConstraintStatement
makeValidScalarConstraint = do
  sc <- generate (sized (genExp @Scalar @R))
  val1 <- VScalar <$> generate arbitrary
  val2 <- VScalar <$> generate arbitrary
  generate $ elements [sc .<= val1, sc .>= val2, sc `between` (val1, val2)]

-- |
prop_constructProblemScalarConstraints :: Expression Scalar R -> Expectation
prop_constructProblemScalarConstraints e = do
  let exp = asRawExpr e
  let vs = varNodesWithShape $ fst exp
  bcs <- mapM makeValidBoxConstraint vs
  sampled <- generate $ sublistOf bcs
  numScalarConstraint <- generate $ elements [2 .. 4]
  scc <- replicateM numScalarConstraint makeValidScalarConstraint
  let constraints = Constraint $ sampled ++ scc
  let constructResult = constructProblem exp constraints
  case constructResult of
    Left reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    Right Problem {..} -> do
      case (scc, scalarConstraints) of
        ([], _) -> return ()
        (_ : _, []) ->
          assertFailure
            "Having scalar constraints but not present in problem"
        (_, sConstraints) -> do
          let isOk sc = length (constraintPartialDerivatives sc) `shouldBe` length variables
          assertBool "Empty constraint ?" $ not (null sConstraints)
          mapM_ isOk sConstraints

-- | List of hand-written problems and the expected result
problemsRepo :: [(Either String Problem, Bool)]
problemsRepo =
  [ ( let [x, y, z, t] = map (variable2D @128 @128) ["x", "y", "z", "t"]
          f = x <.> y + z <.> t
          constraints =
            Constraint
              [ x .>= VFile (TXT "x_lb.txt"),
                y .<= VFile (TXT "y_ub.txt"),
                x <.> z .>= VScalar 3
              ]
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
       in constructProblem f constraints,
      False
    ),
    ( let [x, y, z, t] = map (variable2D @128 @128) ["x", "y", "z", "t"]
          f = x <.> y + z <.> t
          constraints =
            Constraint [x .>= VNum 5, y .<= VNum 10, x <.> z .>= VNum 18]
       in constructProblem f constraints,
      True
    ),
    ( let [x, y, z] = map (variable2D @100 @100) ["x", "y", "z"]
          f = x <.> y + z <.> z
          constraints = Constraint [y <.> z .<= VNum 1]
       in constructProblem f constraints,
      True
    )
  ]

spec :: Spec
spec =
  describe "Hash Solver spec " $ do
    specify "valid problem should be constructed successfully" $
      property prop_constructProblemNoConstraint
    specify "valid box constrained problem should be constructed successfully" $
      property prop_constructProblemBoxConstraint
    specify "valid scalar constraints problem should be successfully successfully" $
      property prop_constructProblemScalarConstraints
