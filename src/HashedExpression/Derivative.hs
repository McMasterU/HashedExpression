{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      :  HashedExpression.Derivative
Copyright   :  (c) OCA 2020
License     :  MIT (see the LICENSE file)
Maintainer  :  anandc@mcmaster.ca
Stability   :  provisional
Portability :  unportable

This module contains all the functionality needed to symbolically compute the derivatives of a 'Expression'. Deriviatives are computed using
Exterior Differential Calculus, a coordinate-independent system of understanding differential systems. There are strong parallels with the
development of algebraic data types and pure functions to tame the complexity and bring transparency to programming. Differential terms
reprented by 'dVar' (dx,dy, etc) often seen as placeholders in calculus are given meaning, resulting in simple algebriac rules to performing
implicit differentiation.

Computing an exterior derivative on an expression @Expression d R@ will result in a @Expression d Covector@, i.e a 'Covector' field
(also known as 1-form). This will contain 'dVar' terms representing where implicit differentiation has occurred. See 'CollectDifferential'
to factor like terms for producing partial derivatives


TODO haddock: do we also sdupport reverse AD?? where??
-}

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
import HashedExpression.Internal.Expression hiding
  ( (*),
    (+),
    (-),
    MultiplyOp (..),
    NumOp (..),
    negate,
  )
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Inner
import HashedExpression.Internal.Node
import HashedExpression.Internal.Normalize
import HashedExpression.Internal.Utils
import HashedExpression.Operation
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
exteriorDerivative :: (DimensionType d) =>
                      -- | Variable Identifiers to take derivative w.r.t
                      Set String ->
                      -- | Expression to take derivative on
                      Expression d R ->
                      -- | Resulting Expression populated with 'DVar' (i.e a 'Covector')
                      Expression d Covector
exteriorDerivative vars = normalize . hiddenDerivative vars . normalize

-- | Same as 'exteriorDerivative' except automatically perform derivative w.r.t all variables. Since derivatives are computed symbolically using
--   exterior algebra, derivatives w.r.t all variables can be represented by a single 'Expression' over a 'Covector' field.
derivativeAllVars :: DimensionType d =>
                     -- | Expression to take derivative on
                     Expression d R ->
                      -- | Resulting Expression populated with 'DVar' (i.e a 'Covector')
                     Expression d Covector
derivativeAllVars expr =
  exteriorDerivative (Set.fromList . map fst $ expressionVarNodes expr) expr

-- | We can write our coerce function because Expression data constructor is exposed, but users can't
coerce :: Expression d1 et1 -> Expression d2 et2
coerce (Expression n mp) = Expression n mp

-- | Creates an 'Expression' that's a singular 'Const' of a specified 'Shape' and value
c :: (DimensionType d) => Shape -> Double -> Expression d R
c shape val = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const val)
    h = hash node

-- | Creates an 'Expression' that's a singular 'Const' of value 1.0
oneOf :: (DimensionType d) => Shape -> Expression d R
oneOf shape = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const 1)
    h = hash node

-- | Hidden computation for exterior derivative
hiddenDerivative :: Set String -> Expression d R -> Expression d Covector
hiddenDerivative vars (Expression n mp) = coerce res
  where
    hiddenDerivative' :: Expression d R -> Expression d Covector
    hiddenDerivative' = hiddenDerivative vars
    exteriorDerivative' = exteriorDerivative vars
    (shape, node) = retrieveInternal n mp
    one = constWithShape @D_ shape 1
    dOne nId = unwrap . hiddenDerivative' $ Expression nId mp
    -- d(g(x)) = g(d(x))
    d1Input :: (Arg -> Node) -> Arg -> Expression d Covector
    d1Input opType arg =
      let df = hiddenDerivative' (Expression arg mp)
       in applyUnary (unary opType) df
    res =
      case node of
        -- dx = dx if x is in vars, otherwise 0
        Var name ->
          -- dc = 0
          let node =
                if Set.member name vars
                  then DVar name
                  else Const 0
              (newMap, h) = fromNode (shape, node)
           in Expression h newMap
        DVar name ->
          error
            "Haven't deal with 1-form yet, only 0-form to 1-form, but this shouldn't be in Expression d R"
        Const _ ->
          let node = Const 0
              (newMap, h) = fromNode (shape, node)
           in Expression h newMap
        -- Sum and multiplication are special cases because they involve multiple arguments
        Sum R args -> wrap . sumMany . map dOne $ args
        -- multiplication rule
        Mul R args ->
          let mkSub nId = (mp, nId)
              dEach (each, rest) = mulMany (map mkSub rest ++ [dOne each])
           in wrap . sumMany . map dEach . removeEach $ args
        -- d(f ^ a) = df * a * f ^ (a - 1)
        Power x arg ->
          let f = Expression @D_ arg mp
              df = hiddenDerivative' f
              constX = constant . fromIntegral $ x
           in constX *. (f ^ (x - 1)) |*| df
        -- d(-f) = -d(f)
        Neg R arg -> d1Input (Neg R) arg
        Scale R arg1 arg2 ->
          let s = Expression @Scalar arg1 mp
              f = Expression @D_ arg2 mp
              ds = hiddenDerivative' s
              df = hiddenDerivative' f
           in ds |*.| f + s *. df
        Div arg1 arg2 ->
          -- d(f / g) = (g / (g * g)) * df - (f / (g * g)) * dg
          let f = Expression @D_ arg1 mp
              g = Expression @D_ arg2 mp
              df = exteriorDerivative' f
              dg = exteriorDerivative' g
              part1 = (g / g ^ 2) |*| df
              part2 = (f / g ^ 2) |*| dg
           in part1 - part2
        Sqrt arg ->
          -- d(sqrt(f)) = 1 / (2 * sqrt(f)) * df
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
              recipSqrtF = constant 0.5 *. (one / sqrt f)
           in recipSqrtF |*| df
        Sin arg ->
          -- d(sin(f)) = cos(f) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in cos f |*| df
        Cos arg ->
          -- d(cos(f)) = -sin(f) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in negate (sin f) |*| df
        Tan arg ->
          -- d(tan(f)) = -1/(cos^2(f)) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
              sqrRecip = one / cos f ^ 2
           in sqrRecip |*| df
        Exp arg ->
          -- d(exp(f)) = exp(f) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in exp f |*| df
        Log arg ->
          -- d(log(f)) = 1 / f * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in (one / f) |*| df
        Sinh arg ->
          -- d(sinh(f)) = cosh(f) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in cosh f |*| df
        Cosh arg ->
          -- d(cosh(f)) = sinh(f) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in sinh f |*| df
        Tanh arg ->
          -- d(tanh(f)) = (1 - tanh^2 h) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in (one - tanh f * tanh f) |*| df
        Asin arg ->
          -- d(asin(f)) = 1 / sqrt(1 - f^2) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in one / sqrt (one - f * f) |*| df
        Acos arg ->
          -- d(acos(f)) = -1 / sqrt(1 - f^2) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in negate (one / sqrt (one - f * f)) |*| df
        Atan arg ->
          -- d(atan(f)) = 1 / (1 + f^2) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in one / (one + f * f) |*| df
        Asinh arg ->
          -- d(asinh(f)) = 1 / sqrt(f^2 + 1) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in one / sqrt (f * f + one) |*| df
        Acosh arg ->
          -- d(acosh(f)) = 1 / sqrt(f^2 - 1) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in one / sqrt (f * f - one) |*| df
        Atanh arg ->
          -- d(atanh(f)) = 1 / sqrt(1 - f^2) * d(f)
          let f = Expression @D_ arg mp
              df = exteriorDerivative' f
           in one / (one - f * f) |*| df
        InnerProd R arg1 arg2 ->
          let f = Expression @D_ arg1 mp
              df = hiddenDerivative' f
              g = Expression @D_ arg2 mp
              dg = hiddenDerivative' g
           in coerce $ f |<.>| dg + g |<.>| df
        Piecewise marks conditionArg branches ->
          let conditionExp = Expression @D_ @R conditionArg mp
              branchExps = map (flip Expression mp) branches
           in piecewise marks conditionExp $
                map hiddenDerivative' branchExps
        Rotate amount arg -> d1Input (Rotate amount) arg
        ReFT arg -> d1Input ReFT arg
        ImFT arg -> d1Input ImFT arg
        TwiceReFT arg -> d1Input TwiceReFT arg
        TwiceImFT arg -> d1Input TwiceImFT arg
        _ -> error $ show node

-- | Element-wise multiply a number with a covector
(|*|) ::
  (DimensionType d) =>
  Expression d R ->
  Expression d Covector ->
  Expression d Covector
(|*|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
  let op = naryET Mul (ElementSpecific Covector) `hasShape` expressionShape e1
   in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Our defined custom dot product with covector - it's more like multiply wise and then add
-- up all the elements
(|<.>|) ::
  (DimensionType d) =>
  Expression d R ->
  Expression d Covector ->
  Expression Scalar Covector
(|<.>|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
  let op = binaryET InnerProd (ElementSpecific Covector) `hasShape` []
   in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Our defined custom scale with Covector, ds |*.| f is like multiply every element of f with ds
(|*.|) ::
  (DimensionType d) =>
  Expression Scalar Covector ->
  Expression d R ->
  Expression d Covector
(|*.|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
  let op = binaryET Scale (ElementSpecific Covector) `hasShape` expressionShape e2
   in applyBinary op e1 e2

infixl 7 |*|

infixl 8 |<.>|, |*.|
