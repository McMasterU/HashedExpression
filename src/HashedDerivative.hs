{-# LANGUAGE ScopedTypeVariables #-}

module HashedDerivative
    ( exteriorDerivative
    ) where

import qualified Data.IntMap.Strict as IM
import Data.List.HT (removeEach)
import HashedExpression
import HashedHash
import HashedOperation
import Prelude hiding ((*), (+), const, cos, sin, sum)

-- | TODO - How can we define more kind of type class constraint to reuse the type-safe operations in HashedOperation ?
-- | TODO - Now, we aren't able to do so, so we just write untyped version of the derivative
--
exteriorDerivative ::
       forall d nt. (DimensionType d)
    => Expression d R
    -> Expression d Covector
exteriorDerivative = hiddenDerivative

-- | Helpers functions for hiddenDerivative
--
dOne :: ExpressionMap -> Int -> (Int, ExpressionMap)
dOne mp n =
    case (length $ retrieveShape n mp, retrieveElementType n mp) of
        (0, R) ->
            unwrap $ hiddenDerivative (Expression n mp :: Expression Zero R)
        (0, C) ->
            unwrap $ hiddenDerivative (Expression n mp :: Expression Zero C)
        (1, R) ->
            unwrap $ hiddenDerivative (Expression n mp :: Expression One R)
        (1, C) ->
            unwrap $ hiddenDerivative (Expression n mp :: Expression One C)
        (2, R) ->
            unwrap $ hiddenDerivative (Expression n mp :: Expression Two R)
        (2, C) ->
            unwrap $ hiddenDerivative (Expression n mp :: Expression Two C)
        (3, R) ->
            unwrap $ hiddenDerivative (Expression n mp :: Expression Three R)
        (3, C) ->
            unwrap $ hiddenDerivative (Expression n mp :: Expression Three C)

arb :: Expression d1 et1 -> Expression d2 et2
arb (Expression n mp) = Expression n mp

-- | Hidden exterior derivative
--
hiddenDerivative ::
       forall d et1 et2. (DimensionType d)
    => Expression d et1
    -> Expression d et2
hiddenDerivative (Expression n mp) =
    let (shape, node) = retrieveInternal n mp
     in case node of
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
            -- | Sum and multiplication are very special case
            Sum R args -- sum rule
                | length args >= 2 -> wrap . sum' . map (dOne mp) $ args
            Mul R args -- multiplication rule
                | length args >= 2 ->
                    let mkSub nId = (nId, mp)
                        dEach (one, rest) =
                            mul' (map mkSub rest ++ [dOne mp one])
                     in wrap . sum' . map dEach . removeEach $ args
            Sin arg
                -- d(sin(f x)) = cos (f x) * d(f x)
             ->
                let fx = Expression arg mp :: Expression d R
                    dFx = hiddenDerivative fx :: Expression d Covector
                    cosFx = cos fx
                 in arb $ cosFx |*| dFx
            Cos arg
                -- d(cos(f x)) = -sin (f x) * d(f x)
             ->
                let fx = Expression arg mp :: Expression d R
                    dFx = hiddenDerivative fx :: Expression d Covector
                    minusSinFx = const (-1) `scale` sin fx
                 in arb $ minusSinFx |*| dFx


-- |
--
unwrap :: Expression d et -> (Int, ExpressionMap)
unwrap (Expression n mp) = (n, mp)

wrap :: (Int, ExpressionMap) -> Expression d et
wrap = uncurry Expression

-- | General multiplication
--
highestShape :: [(Int, ExpressionMap)] -> Shape
highestShape = foldl f []
  where
    f acc (n, mp) =
        if length acc > length (retrieveShape n mp)
            then acc
            else retrieveShape n mp

highestElementType :: [(Int, ExpressionMap)] -> ET
highestElementType = foldl f R
  where
    f acc (n, mp) = max acc (retrieveElementType n mp) -- R < C < Covector (TODO - ad hoc?)

mul' :: [(Int, ExpressionMap)] -> (Int, ExpressionMap)
mul' es = (h, newMap)
  where
    elementType = highestElementType es
    shape = highestShape es
    node = Mul elementType . map fst $ es
    mergedMap = foldl1 IM.union . map snd $ es
    (newMap, h) = addEdge mergedMap (shape, node)

-- | General sum
--
sum' :: [(Int, ExpressionMap)] -> (Int, ExpressionMap)
sum' es = (h, newMap)
  where
    (n, mp) = head es
    elementType = retrieveElementType n mp
    shape = retrieveShape n mp
    node = Sum elementType . map fst $ es
    mergedMap = foldl1 IM.union . map snd $ es
    (newMap, h) = addEdge mergedMap (shape, node)

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
