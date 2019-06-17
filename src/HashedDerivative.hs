{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}

module HashedDerivative
    ( exteriorDerivative
    ) where

import qualified Data.IntMap.Strict as IM
import Data.List.HT (removeEach)
import Data.Typeable (Typeable)
import HashedExpression
import HashedUtils
import HashedHash
import HashedOperation
import Prelude hiding ((*), (+), (/), const, cos, exp, log, sin, sqrt, sum)

-- | Exterior derivative
--
exteriorDerivative ::
       (DimensionType d) => Expression d R -> Expression d Covector
exteriorDerivative = hiddenDerivative

-- |
--
data WhateverD
    deriving (Typeable, DimensionType)

-- | We can write our coerce function because Expression data constructor is exposed, but users can't
--
coerce :: Expression d1 et1 -> Expression d2 et2
coerce (Expression n mp) = Expression n mp

-- | Hidden const to represent many dimension
--
const' :: (DimensionType d) => Shape -> Double -> Expression d R
const' shape val = Expression h (IM.fromList [(h, node)])
  where
    node = (shape, Const val)
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
        d1Input op arg =
            let df = hiddenDerivative (Expression arg mp)
                dfShape = expressionShape df
                outputNode = op (exIndex df)
                (newMap, nRes) = addEdge (exMap df) (dfShape, outputNode)
             in Expression nRes newMap
        -- For cases g = RealImag, .. that take 2 input
        -- d(g(x, y)) = g(d(x), d(y))
        d2Input :: (Arg -> Arg -> Node) -> Arg -> Arg -> Expression d2 et2
        d2Input op arg1 arg2 =
            let df1 = hiddenDerivative (Expression arg1 mp)
                df2 = hiddenDerivative (Expression arg2 mp)
                dfShape = expressionShape df1
                outputNode = op (exIndex df1) (exIndex df2)
                (newMap, nRes) =
                    addEdge
                        (exMap df1 `IM.union` exMap df2)
                        (dfShape, outputNode)
             in Expression nRes newMap
        res =
            case node of
                Var name ->
                    let node = DVar name
                        (newMap, h) = fromNode (shape, node)
                -- dx = dx
                     in Expression h newMap
                Const _ ->
                    let node = Const 0
                        (newMap, h) = fromNode (shape, node)
                -- dc = 0
                     in Expression h newMap
            -- | Sum and multiplication are special cases because they involve multiple arguments
                Sum _ args -> wrap . sum' . map dOne $ args
                Mul _ args -- multiplication rule
                    | length args >= 2 ->
                        let mkSub nId = (nId, mp)
                            dEach (one, rest) =
                                mul' (map mkSub rest ++ [dOne one])
                         in wrap . sum' . map dEach . removeEach $ args
                Div arg1 arg2
                -- d (f / g) = (g / (g * g)) * df - (f / (g * g)) * dg
                 ->
                    let f = Expression arg1 mp :: Expression WhateverD R
                        g = Expression arg2 mp :: Expression WhateverD R
                        df = exteriorDerivative f
                        dg = exteriorDerivative g
                        g'2 = g * g
                        part1 = (g / g'2) |*| df
                        part2 = const (-1) *. (f / g'2) |*| dg
                     in part1 + part2
                Sqrt arg
                -- d(sqrt(f)) = 1 / (2 * sqrt(f)) * df
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                        recipSqrtF = const' (expressionShape f) 0.5 / sqrt f
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
                        minusSinFx = const (-1) *. sin f
                     in minusSinFx |*| df
                Tan arg
                -- d(tan(f)) = -1/(cos^2(f)) * d(f)
                 ->
                    let f = Expression arg mp :: Expression WhateverD R
                        df = exteriorDerivative f
                        cosSqrF = cos f * cos f
                        sqrRecip = const' shape 1 / cosSqrF
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
                     in const' shape 1 / f |*| df
                Sinh arg -> undefined
                Cosh arg -> undefined
                Tanh arg -> undefined
                Asin arg -> undefined
                Acos arg -> undefined
                Atan arg -> undefined
                Asinh arg -> undefined
                Acosh arg -> undefined
                Atanh arg -> undefined
                RealPart arg -> d1Input RealPart arg
                ImagPart arg -> d1Input ImagPart arg
                RealImag arg1 arg2 -> d2Input RealImag arg1 arg2
     in coerce res


-- | Wise-scale R with a covector
--
(|*|) ::
       (DimensionType d)
    => Expression d R
    -> Expression d Covector
    -> Expression d Covector
(|*|) e1@(Expression n1 mp1) e2@(Expression n2 mp2) =
    ensureSameShape e1 e2 $ Expression h newMap
  where
    elementType = expressionElementType e1
    shape = expressionShape e1
    node = Mul elementType [n1, n2]
    (newMap, h) = addEdge (mp1 `IM.union` mp2) (shape, node)
