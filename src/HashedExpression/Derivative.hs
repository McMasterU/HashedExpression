{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------
-- |
-- This module is for computing exterior derivative
--
-------------------------------------------------------------------------------
module HashedExpression.Derivative
    ( exteriorDerivative
    , derivativeAllVars
    ) where

import qualified Data.IntMap.Strict as IM
import Data.List.HT (removeEach)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import HashedExpression.Expression

import HashedExpression.Hash
import HashedExpression.Inner
import HashedNode
import HashedNormalize
import HashedOperation
import HashedExpression.Utils
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , (^)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , const
    , cos
    , cosh
    , exp
    , log
    , negate
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )

-- | Exterior derivative
--
exteriorDerivative ::
       (DimensionType d)
    => Set String -- ^ Variables
    -> Expression d R -- ^ Expression
    -> Expression d Covector
exteriorDerivative vars = normalize . hiddenDerivative vars . normalize

-- | Take derivative with all vars
--
derivativeAllVars :: DimensionType d => Expression d R -> Expression d Covector
derivativeAllVars expr =
    exteriorDerivative (Set.fromList . map fst $ expressionVarNodes expr) expr

-- | We can write our coerce function because Expression data constructor is exposed, but users can't
--
coerce :: Expression d1 et1 -> Expression d2 et2
coerce (Expression n mp) = Expression n mp

-- | Hidden const to represent many dimensions
--
c :: (DimensionType d) => Shape -> Double -> Expression d R
c shape val = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const val)
    h = hash node

oneOf :: (DimensionType d) => Shape -> Expression d R
oneOf shape = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const 1)
    h = hash node

-- | Hidden exterior derivative
--
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
        case node
                -- dx = dx if x is in vars, otherwise 0
              of
            Var name
                -- dc = 0
             ->
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
                    constX = const . fromIntegral $ x
                 in constX *. (f ^ (x - 1)) |*| df
                 -- d(-f) = -d(f)
            Neg R arg -> d1Input (Neg R) arg
            Scale R arg1 arg2 ->
                let s = Expression @Scalar arg1 mp
                    f = Expression @D_ arg2 mp
                    ds = hiddenDerivative' s
                    df = hiddenDerivative' f
                 in ds |*.| f + s *. df
            Div arg1 arg2
                -- d(f / g) = (g / (g * g)) * df - (f / (g * g)) * dg
             ->
                let f = Expression @D_ arg1 mp
                    g = Expression @D_ arg2 mp
                    df = exteriorDerivative' f
                    dg = exteriorDerivative' g
                    part1 = (g / g ^ 2) |*| df
                    part2 = (f / g ^ 2) |*| dg
                 in part1 - part2
            Sqrt arg
                -- d(sqrt(f)) = 1 / (2 * sqrt(f)) * df
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                    recipSqrtF = const 0.5 *. (one / sqrt f)
                 in recipSqrtF |*| df
            Sin arg
                -- d(sin(f)) = cos(f) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in cos f |*| df
            Cos arg
                -- d(cos(f)) = -sin(f) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in negate (sin f) |*| df
            Tan arg
                -- d(tan(f)) = -1/(cos^2(f)) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                    sqrRecip = one / cos f ^ 2
                 in sqrRecip |*| df
            Exp arg
                -- d(exp(f)) = exp(f) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in exp f |*| df
            Log arg
                -- d(log(f)) = 1 / f * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in (one / f) |*| df
            Sinh arg
                -- d(sinh(f)) = cosh(f) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in cosh f |*| df
            Cosh arg
                -- d(cosh(f)) = sinh(f) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in sinh f |*| df
            Tanh arg
                -- d(tanh(f)) = (1 - tanh^2 h) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in (one - tanh f * tanh f) |*| df
            Asin arg
                -- d(asin(f)) = 1 / sqrt(1 - f^2) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in one / sqrt (one - f * f) |*| df
            Acos arg
                -- d(acos(f)) = -1 / sqrt(1 - f^2) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in negate (one / sqrt (one - f * f)) |*| df
            Atan arg
                -- d(atan(f)) = 1 / (1 + f^2) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in one / (one + f * f) |*| df
            Asinh arg
                -- d(asinh(f)) = 1 / sqrt(f^2 + 1) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in one / sqrt (f * f + one) |*| df
            Acosh arg
                -- d(acosh(f)) = 1 / sqrt(f^2 - 1) * d(f)
             ->
                let f = Expression @D_ arg mp
                    df = exteriorDerivative' f
                 in one / sqrt (f * f - one) |*| df
            Atanh arg
                -- d(atanh(f)) = 1 / sqrt(1 - f^2) * d(f)
             ->
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

-------------------------------------------------------------------------------
-- |
--
--
-------------------------------------------------------------------------------
-- | Wise-multiply a number with a covector
--
(|*|) ::
       (DimensionType d)
    => Expression d R
    -> Expression d Covector
    -> Expression d Covector
(|*|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    let op = naryET Mul (ElementSpecific Covector) `hasShape` expressionShape e1
     in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Our defined custom dot product with covector - it's more like multiply wise and then add
-- up all the elements
(|<.>|) ::
       (DimensionType d)
    => Expression d R
    -> Expression d Covector
    -> Expression Scalar Covector
(|<.>|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    let op = binaryET InnerProd (ElementSpecific Covector) `hasShape` []
     in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Our defined custom scale with Covector, ds |*.| f is like multiply every element of f with ds
--
(|*.|) ::
       (DimensionType d)
    => Expression Scalar Covector
    -> Expression d R
    -> Expression d Covector
(|*.|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    let op =
            binaryET Scale (ElementSpecific Covector) `hasShape`
            expressionShape e2
     in applyBinary op e1 e2

infixl 7 |*|

infixl 8 |<.>|, |*.|
