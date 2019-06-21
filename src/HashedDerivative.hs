{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

-------------------------------------------------------------------------------
-- |
-- This module is for computing exterior derivative
--
-------------------------------------------------------------------------------
module HashedDerivative
    ( exteriorDerivative
    ) where

import qualified Data.IntMap.Strict as IM
import Data.List.HT (removeEach)
import Data.Typeable (Typeable)
import HashedExpression
import HashedHash
import HashedInner
import HashedOperation
import HashedUtils
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
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
       (DimensionType d) => Expression d R -> Expression d Covector
exteriorDerivative = hiddenDerivative

-- |
--
data WhateverD
    deriving (Typeable, DimensionType)

data WhateverNT
    deriving (Typeable, ElementType, NumType)

-- | We can write our coerce function because Expression data constructor is exposed, but users can't
--
coerce :: Expression d1 et1 -> Expression d2 et2
coerce (Expression n mp) = Expression n mp

-- | Hidden const to represent many dimension
--
c :: (DimensionType d) => Shape -> Double -> Expression d R
c shape val = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const val)
    h = hash node

one :: (DimensionType d) => Shape -> Expression d R
one shape = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const 1)
    h = hash node

-- | Hidden exterior derivative
--
hiddenDerivative :: Expression d1 et1 -> Expression d2 et2
hiddenDerivative (Expression n mp) =
    let (shape, node) = retrieveInternal n mp
        dOne nId = unwrap . hiddenDerivative $ Expression nId mp
        -- For cases g = ImagPart, RealPart, FFT, .. that take 1 input
        -- d(g(x)) = g(d(x))
        d1Input :: (Arg -> Node) -> Arg -> Expression d2 et2
        d1Input opType arg =
            let df = hiddenDerivative (Expression arg mp)
             in applyUnary (unary opType) df
        -- For cases g = RealImag, .. that take 2 input
        -- d(g(x, y)) = g(d(x), d(y))
        d2Input :: (Arg -> Arg -> Node) -> Arg -> Arg -> Expression d2 et2
        d2Input opType arg1 arg2 =
            let df1 = hiddenDerivative (Expression arg1 mp)
                df2 = hiddenDerivative (Expression arg2 mp)
             in applyBinary (binary opType) df1 df2
        res =
            case node
                -- dx = dx
                  of
                Var name ->
                    let node = DVar name
                        (newMap, h) = fromNode (shape, node)
                     in Expression h newMap
                -- dc = 0
                DVar name ->
                    error
                        "Haven't deal with 1-form yet, only 0-form to 1-form, but this shouldn't be in Expression d R"
                Const _ ->
                    let node = Const 0
                        (newMap, h) = fromNode (shape, node)
                     in Expression h newMap
                -- Sum and multiplication are special cases because they involve multiple arguments
                Sum _ args -> wrap . sumMany . map dOne $ args
                -- multiplication rule
                Mul _ args ->
                    let mkSub nId = (mp, nId)
                        dEach (one, rest) =
                            mulMany (map mkSub rest ++ [dOne one])
                     in wrap . sumMany . map dEach . removeEach $ args
                -- d(-f) = -d(f)
                Neg et arg -> d1Input (Neg et) arg
                Scale et arg1 arg2 -> error "Haven't implemented"
                Div arg1 arg2
                -- d(f / g) = (g / (g * g)) * df - (f / (g * g)) * dg
                 ->
                    let f = Expression arg1 mp :: Expression WhateverD R
                        g = Expression arg2 mp :: Expression WhateverD R
                        df = exteriorDerivative f
                        dg = exteriorDerivative g
                        g'2 = g * g
                        part1 = (g / g'2) |*| df
                        part2 = (f / g'2) |*| dg
                     in part1 - part2
                Sqrt arg
                -- d(sqrt(f)) = 1 / (2 * sqrt(f)) * df
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                        recipSqrtF = c (expressionShape f) 0.5 / sqrt f
                     in recipSqrtF |*| df
                Sin arg
                -- d(sin(f)) = cos(f) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in cos f |*| df
                Cos arg
                -- d(cos(f)) = -sin(f) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in negate (sin f) |*| df
                Tan arg
                -- d(tan(f)) = -1/(cos^2(f)) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                        cosSqrF = cos f * cos f
                        sqrRecip = one shape / cosSqrF
                     in sqrRecip |*| df
                Exp arg
                -- d(exp(f)) = exp(f) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in exp f |*| df
                Log arg
                -- d(log(f)) = 1 / f * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in one shape / f |*| df
                Sinh arg
                -- d(sinh(f)) = cosh(f) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in cosh f |*| df
                Cosh arg
                -- d(cosh(f)) = sinh(f) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in sinh f |*| df
                Tanh arg
                -- d(tanh(f)) = (1 - tanh^2 h) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in (one shape - tanh f * tanh f) |*| df
                Asin arg
                -- d(asin(f)) = 1 / sqrt(1 - f^2) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in one shape / sqrt (one shape - f * f) |*| df
                Acos arg
                -- d(acos(f)) = -1 / sqrt(1 - f^2) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in negate (one shape / sqrt (one shape - f * f)) |*| df
                Atan arg
                -- d(atan(f)) = 1 / (1 + f^2) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in one shape / (one shape + f * f) |*| df
                Asinh arg
                -- d(asinh(f)) = 1 / sqrt(f^2 + 1) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in one shape / sqrt (f * f + one shape) |*| df
                Acosh arg
                -- d(acosh(f)) = 1 / sqrt(f^2 - 1) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in one shape / sqrt (f * f - one shape) |*| df
                Atanh arg
                -- d(atanh(f)) = 1 / sqrt(1 - f^2) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                     in one shape / (one shape - f * f) |*| df
                -- d(xRe(f)) = xRe(d(f))
                RealPart arg -> d1Input RealPart arg
                -- d(xIm(f)) = xIm(d(f))
                ImagPart arg -> d1Input ImagPart arg
                -- d(x +: y) = d(x) :+ d(y)
                RealImag arg1 arg2 -> d2Input RealImag arg1 arg2
                InnerProd et arg1 arg2 ->
                    let f =
                            Expression arg1 mp :: Expression WhateverD WhateverNT
                        df = hiddenDerivative f :: Expression WhateverD Covector
                        g =
                            Expression arg2 mp :: Expression WhateverD WhateverNT
                        dg = hiddenDerivative g :: Expression WhateverD Covector
                     in coerce $ f |<.>| dg + g |<.>| df
     in coerce res

-- | Wise-multiply a number with a covector
--
(|*|) ::
       (DimensionType d, NumType nt)
    => Expression d nt
    -> Expression d Covector
    -> Expression d Covector
(|*|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    let op = naryET Mul (ElementSpecific Covector) `hasShape` expressionShape e1
     in ensureSameShape e1 e2 $ applyBinary op e1 e2

-- | Our defined custom dot product with covector - it's more like multiply wise and then add
-- up all the elements
(|<.>|) ::
       (DimensionType d, NumType nt)
    => Expression d nt
    -> Expression d Covector
    -> Expression Zero Covector
(|<.>|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    let op = binaryET InnerProd (ElementSpecific Covector) `hasShape` []
     in ensureSameShape e1 e2 $ applyBinary op e1 e2

infixl 8 |*|, |<.>|
