{-# LANGUAGE ScopedTypeVariables #-}

module HashedDerivative2
    ( exteriorDerivative
    ) where

import qualified Data.IntMap.Strict as IM
import Data.List.HT (removeEach)
import HashedExpression
import HashedHash
import HashedOperation
import Prelude hiding ((*), (+), sum)

-- | TODO - How can we define more kind of type class constraint to reuse the type-safe operations in HashedOperation ?
-- | TODO - Now, we aren't able to do so, so we just write untyped version of the derivative
--
exteriorDerivative ::
       forall d. (DimensionType d)
    => Expression d R
    -> Expression d Covector
exteriorDerivative = toTyped . hiddenExteriorDerivative . toUntyped

-- | Untyped version for computing derivative
--
data UntypedExpression =
    UntypedExpression
        { uIndex :: Int
        , uMap :: ExpressionMap
        }

toTyped ::
       (DimensionType d, ElementType et) => UntypedExpression -> Expression d et
toTyped (UntypedExpression n mp) = Expression n mp

toUntyped ::
       (DimensionType d, ElementType et) => Expression d et -> UntypedExpression
toUntyped (Expression n mp) = UntypedExpression n mp

-- | General multiplication
--
highestShape :: [UntypedExpression] -> Shape
highestShape = foldl f []
  where
    f acc (UntypedExpression n mp) =
        if length acc > length (retrieveShape n mp)
            then acc
            else retrieveShape n mp

highestElementType :: [UntypedExpression] -> ET
highestElementType = foldl f R
  where
    f acc (UntypedExpression n mp) = max acc (retrieveElementType n mp) -- R < C < Covector (TODO - ad hoc?)

generalMul :: [UntypedExpression] -> UntypedExpression
generalMul es = UntypedExpression h newMap
  where
    elementType = highestElementType es
    shape = highestShape es
    node = Mul elementType . map uIndex $ es
    mergedMap = foldl1 IM.union . map uMap $ es
    (newMap, h) = addEdge mergedMap (shape, node)

-- | General sum
--
generalSum :: [UntypedExpression] -> UntypedExpression
generalSum es = UntypedExpression h newMap
  where
    UntypedExpression n mp = head es
    elementType = retrieveElementType n mp
    shape = retrieveShape n mp
    node = Sum elementType . map uIndex $ es
    mergedMap = foldl1 IM.union . map uMap $ es
    (newMap, h) = addEdge mergedMap (shape, node)

-- | (x, [y, z, ..]) = (y * z * .. ) * dx
--
combineDerivative ::
       (UntypedExpression, [UntypedExpression]) -> UntypedExpression
combineDerivative (exp, []) = hiddenExteriorDerivative exp
combineDerivative (exp, [otherExp]) =
    generalMul [otherExp, hiddenExteriorDerivative exp]
combineDerivative (exp, expRest) =
    generalMul [generalMul expRest, hiddenExteriorDerivative exp]

hiddenExteriorDerivative :: UntypedExpression -> UntypedExpression
hiddenExteriorDerivative e@(UntypedExpression n mp) =
    let (shape, node) = retrieveInternal n mp
     in case node of
            Var name ->
                let node = DVar name
                    (newMap, h) = fromNode (shape, node)
                -- dx = dx
                 in UntypedExpression h newMap
            Const _ ->
                let node = Const 0
                    (newMap, h) = fromNode (shape, node)
                -- dc = 0
                 in UntypedExpression h newMap
            Sum R args -- sum rule
                | length args >= 2 ->
                    generalSum .
                    map (hiddenExteriorDerivative . flip UntypedExpression mp) $
                    args
            Mul R args -- multiplication rule
                | length args >= 2 ->
                    let mkExp = flip UntypedExpression mp
                        mkExpsEach (nId, rest) = (mkExp nId, map mkExp rest)
                     in generalSum .
                        map (combineDerivative . mkExpsEach) . removeEach $
                        args
