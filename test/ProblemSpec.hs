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
import HashedExpression.Interface
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
prop_constructProblemNoConstraint :: TypedExpr Scalar R -> Expectation
prop_constructProblemNoConstraint exp = do
  let constructResult = constructProblem exp []
  case constructResult of
    Left reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    Right Problem {..} -> do
      return ()

-- |
prop_constructProblemBoxConstraint :: TypedExpr Scalar R -> Expectation
prop_constructProblemBoxConstraint e = do
  let exp = asRawExpr e
  let vars = varsWithShape $ fst exp
  boxConstraints <-
    generate $
      sublistOf $
        concatMap
          ( \(name, _) ->
              [ BoxLowerDecl name (name <> "_lb"),
                BoxUpperDecl name (name <> "_lb")
              ]
          )
          vars
  let constructResult = constructProblem exp boxConstraints
  case constructResult of
    Left reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    Right Problem {..} -> return ()

-- -- |
makeValidScalarConstraint :: IO ConstraintDecl
makeValidScalarConstraint = do
  sc <- generate (sized (genExp @Scalar @R))
  val1 <- generate $ arbitrary @Double
  val2 <- generate $ arbitrary @Double
  generate $ elements [sc .<= val1, sc .>= val2]

-- |
prop_constructProblemScalarConstraints :: TypedExpr Scalar R -> Expectation
prop_constructProblemScalarConstraints e = do
  let exp = asRawExpr e
  let vars = varsWithShape $ fst exp
  boxConstraints <-
    generate $
      sublistOf $
        concatMap
          ( \(name, _) ->
              [ BoxLowerDecl name (name <> "_lb"),
                BoxUpperDecl name (name <> "_lb")
              ]
          )
          vars
  numScalarConstraint <- generate $ elements [2 .. 4]
  scc <- replicateM numScalarConstraint makeValidScalarConstraint
  case constructProblem exp (scc ++ boxConstraints) of
    Left reason ->
      assertFailure $ "Can't construct problem: " ++ reason
    Right Problem {..} -> do
      case (scc, generalConstraints) of
        ([], _) -> return ()
        (_ : _, []) -> assertFailure "Having scalar constraints but not present in problem"
        (_, sConstraints) -> do
          let isOk sc = length (constraintPartialDerivatives sc) `shouldBe` length variables
          assertBool "Empty constraint ?" $ not (null sConstraints)
          mapM_ isOk sConstraints

spec :: Spec
spec =
  describe "Hash Solver spec " $ do
    specify "valid problem should be constructed successfully" $
      property prop_constructProblemNoConstraint
    specify "valid box constrained problem should be constructed successfully" $
      property prop_constructProblemBoxConstraint
    specify "valid scalar constraints problem should be successfully successfully" $
      property prop_constructProblemScalarConstraints
