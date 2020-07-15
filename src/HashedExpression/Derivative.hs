{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  HashedExpression.Derivative
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module contains all the functionality needed to symbolically compute the derivatives of a 'Expression'. Deriviatives are computed using
-- Exterior Differential Calculus, a coordinate-independent system of understanding differential systems. There are strong parallels with the
-- development of algebraic data types and pure functions to tame the complexity and bring transparency to programming. Differential terms
-- reprented by 'dVar' (dx,dy, etc) often seen as placeholders in calculus are given meaning, resulting in simple algebriac rules to performing
-- implicit differentiation.
--
-- Computing an exterior derivative on an expression @Expression d R@ will result in a @Expression d Covector@, i.e a 'Covector' field
-- (also known as 1-form). This will contain 'dVar' terms representing where implicit differentiation has occurred. See 'CollectDifferential'
-- to factor like terms for producing partial derivatives
module HashedExpression.Derivative
  ( exteriorDerivative,
    derivativeAllVars,
  )
where

import qualified Data.IntMap.Strict as IM
import Data.List.HT (removeEach)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Debug.Trace (traceShow, traceShowId)
import HashedExpression.Internal
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Utils
import HashedExpression.Operation
import HashedExpression.Prettify
import Prelude hiding ((^))

-- | Compute the exterior derivative w.r.t the given variable identifiers (the 'String' wrapped by 'Var'). This transforms a real expression
--   @Expression d R@ into @Expression d Covector@ (an expression with 'DVar' terms). This is because we compute derivatives symbolically
--   using exterior algebra, so derivatives w.r.t all variables can be represented by a single 'Expression' over a 'Covector' field. For example,
--
--  >>> let [x,y] = [variable "x",variable "y"]
--  >>> prettify $ exteriorDerivative (Set.fromList ["x","y"]) (2*x + y)
--  "((2.0*dx)+dy) :: Covector"
--
--  Note: the partial derivatives are the terms scaling the differential variables (i.e 'DVar',dx,dy,etc), however you may need to factor them
--  first using 'collectDifferentials'
exteriorDerivative ::
  (DimensionType d) =>
  -- | Variable Identifiers to take derivative w.r.t
  Set String ->
  -- | Expression to take derivative on
  Expression d R ->
  -- | Resulting Expression populated with 'DVar' (i.e a 'Covector')
  Expression d Covector
exteriorDerivative vars = normalize . hiddenDerivative vars . normalize

-- | Same as 'exteriorDerivative' except automatically perform derivative w.r.t all variables. Since derivatives are computed symbolically using
--   exterior algebra, derivatives w.r.t all variables can be represented by a single 'Expression' over a 'Covector' field.
derivativeAllVars ::
  DimensionType d =>
  -- | Expression to take derivative on
  Expression d R ->
  -- | Resulting Expression populated with 'DVar' (i.e a 'Covector')
  Expression d Covector
derivativeAllVars expr =
  exteriorDerivative (Set.fromList . map fst $ expressionVarNodes expr) expr

-- | We can write our coerce function because Expression data constructor is exposed, but users can't
coerce :: Expression d1 et1 -> Expression d2 et2
coerce (Expression n mp) = Expression n mp

-- | Hidden computation for exterior derivative
hiddenDerivative :: Set String -> Expression d R -> Expression d Covector
hiddenDerivative vars (Expression n mp) = coerce res
  where
    (shape, R, node) = retrieveNode n mp
    -------------------------------------------------------------------------------
    d :: Expression d R -> Expression d Covector
    d = hiddenDerivative vars
    -------------------------------------------------------------------------------
    commute :: UnarySpec -> Arg -> Expression d Covector
    commute spec arg =
      let df = d (Expression arg mp)
       in applyUnary spec df
    -------------------------------------------------------------------------------
    asR :: NodeID -> Expression D_ R
    asR nID = Expression nID mp
    -------------------------------------------------------------------------------
    asScalarR :: NodeID -> Expression Scalar R
    asScalarR nID = Expression nID mp
    -------------------------------------------------------------------------------
    one :: Expression D_ R
    one = constWithShape shape 1
    -------------------------------------------------------------------------------
    res :: Expression D_ Covector
    res =
      case node of
        -- dx = dx if x is in vars, otherwise dzero
        Var name -> fromNode (shape, Covector, if Set.member name vars then DVar name else DZero)
        Const _ -> fromNode (shape, Covector, DZero)
        Sum args -> applyNary specSum $ map (d . asR) args
        Mul args ->
          let process :: (NodeID, [NodeID]) -> Expression D_ Covector
              process (x, rest) = applyNary specMul (map asR rest) |*| d (asR x)
           in applyNary specSum $ map process $ removeEach args
        -- d(f ^ a) = df * a * f ^ (a - 1)
        Power x arg ->
          let f = asR arg
              df = d f
              constX = constant . fromIntegral $ x
           in (constX *. (f ^ (x - 1))) |*| df
        -- d(-f) = -d(f)
        Neg arg ->
          let f = asR arg
              df = d f
           in (- one) |*| df
        Scale arg1 arg2 ->
          let s = asScalarR arg1
              f = asR arg2
              ds = d s
              df = d f
           in ds |.*| f + s |*.| df
        Div arg1 arg2 ->
          -- d(f / g) = (g / (g * g)) * df - (f / (g * g)) * dg
          let f = asR arg1
              g = asR arg2
              df = d f
              dg = d g
              part1 = (g / g ^ 2) |*| df
              part2 = (f / g ^ 2) |*| dg
           in part1 - part2
        Sqrt arg ->
          -- d(sqrt(f)) = 1 / (2 * sqrt(f)) * df
          let f = asR arg
              df = d f
              recipSqrtF = constant 0.5 *. (one / sqrt f)
           in recipSqrtF |*| df
        Sin arg ->
          -- d(sin(f)) = cos(f) * d(f)
          let f = asR arg
              df = d f
           in cos f |*| df
        Cos arg ->
          -- d(cos(f)) = -sin(f) * d(f)
          let f = asR arg
              df = d f
           in negate (sin f) |*| df
        Tan arg ->
          -- d(tan(f)) = -1/(cos^2(f)) * d(f)
          let f = asR arg
              df = d f
              sqrRecip = one / cos f ^ 2
           in sqrRecip |*| df
        Exp arg ->
          -- d(exp(f)) = exp(f) * d(f)
          let f = asR arg
              df = d f
           in exp f |*| df
        Log arg ->
          -- d(log(f)) = 1 / f * d(f)
          let f = asR arg
              df = d f
           in (one / f) |*| df
        Sinh arg ->
          -- d(sinh(f)) = cosh(f) * d(f)
          let f = asR arg
              df = d f
           in cosh f |*| df
        Cosh arg ->
          -- d(cosh(f)) = sinh(f) * d(f)
          let f = asR arg
              df = d f
           in sinh f |*| df
        Tanh arg ->
          -- d(tanh(f)) = (1 - tanh^2 h) * d(f)
          let f = asR arg
              df = d f
           in (one - tanh f * tanh f) |*| df
        Asin arg ->
          -- d(asin(f)) = 1 / sqrt(1 - f^2) * d(f)
          let f = asR arg
              df = d f
           in (one / sqrt (one - f * f)) |*| df
        Acos arg ->
          -- d(acos(f)) = -1 / sqrt(1 - f^2) * d(f)
          let f = asR arg
              df = d f
           in negate (one / sqrt (one - f * f)) |*| df
        Atan arg ->
          -- d(atan(f)) = 1 / (1 + f^2) * d(f)
          let f = asR arg
              df = d f
           in one / (one + f * f) |*| df
        Asinh arg ->
          -- d(asinh(f)) = 1 / sqrt(f^2 + 1) * d(f)
          let f = asR arg
              df = d f
           in one / sqrt (f * f + one) |*| df
        Acosh arg ->
          -- d(acosh(f)) = 1 / sqrt(f^2 - 1) * d(f)
          let f = asR arg
              df = d f
           in one / sqrt (f * f - one) |*| df
        Atanh arg ->
          -- d(atanh(f)) = 1 / sqrt(1 - f^2) * d(f)
          let f = asR arg
              df = d f
           in one / (one - f * f) |*| df
        InnerProd arg1 arg2 ->
          let f = asR arg1
              df = d f
              g = asR arg2
              dg = d g
           in coerce $ f |<.>| dg + g |<.>| df
        Piecewise marks conditionArg branches ->
          let conditionExp = asR conditionArg
              branchExps = map (flip Expression mp) branches
           in piecewise marks conditionExp $ map d branchExps
        -- Operations commute with taking differentials
        Rotate amount arg -> commute (specRotate amount) arg
        ReFT arg -> commute specReFT arg
        ImFT arg -> commute specImFT arg
        TwiceReFT arg -> commute specTwiceReFT arg
        TwiceImFT arg -> commute specTwiceImFT arg
        _ -> error $ show node
