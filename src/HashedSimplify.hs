-------------------------------------------------------------------------------
-- | For simplifying expressions
--
-------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module HashedSimplify where

import Control.Arrow ((>>>))
import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (group, groupBy)
import Data.List.NonEmpty (groupWith)
import qualified Data.Map.Strict as Map
import HashedExpression
import HashedHash
import HashedInner
import HashedNode
import HashedOperation (const, const1d, const2d, const3d, plus, times)
import HashedPattern
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
    , product
    , sin
    , sinh
    , sqrt
    , sum
    , tan
    , tanh
    )
import qualified Prelude

-- | Simplification type, we can combine them, chain them, apply them n times using nest, ...
--
type Simplification = (ExpressionMap, Int) -> (ExpressionMap, Int)

-- | Chain n simplifications together to a simplification
--
chain :: [a -> a] -> a -> a
chain = flip $ foldl (|>)

-- | Apply maximum k times, or stop if the expression doesn't change
--
multipleTimes :: Int -> Simplification -> Simplification
multipleTimes k smp exp = go (k - 1) exp (smp exp)
  where
    go 0 _ curExp = curExp
    go k lastExp curExp
        | snd lastExp == snd curExp = curExp
        | otherwise = go (k - 1) curExp (smp curExp)

-- | Simplify an expression
--
simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e =
    let applyRules =
            multipleTimes 100 $
            (makeRecursive standardize) >>>
            rulesFromPattern >>>
            (makeRecursive reduceSumProdRules) >>>
            (makeRecursive groupConstantsRules) >>>
            (makeRecursive combineTermsRules)
     in wrap . removeUnreachable . applyRules . unwrap $ e

rulesFromPattern :: Simplification
rulesFromPattern =
    makeRecursive . chain . map fromPattern $
    zeroOneRules ++
    scaleRules ++
    complexNumRules ++
    dotProductRules ++
    distributiveRules ++ piecewiseRules ++ exponentRules ++ otherRules

-- | Rules with zero and one
--
zeroOneRules :: [(GuardedPattern, Pattern)]
zeroOneRules =
    [ one *. x |.~~~~~~> x
    , one * x |.~~~~~~> x
    , x * one |.~~~~~~> x
    , zero * x |.~~~~~~> zero
    , x * zero |.~~~~~~> zero
    , zero *. x |. isReal x ~~~~~~> zero
    , zero *. x |. isComplex x ~~~~~~> zero +: zero
    , x *. zero |.~~~~~~> zero
    , one *. x |.~~~~~~> x
    , x + zero |.~~~~~~> x
    , zero + x |.~~~~~~> x
    , (x <.> zero) |.~~~~~~> zero
    , zero <.> x |.~~~~~~> zero
    , negate zero |.~~~~~~> zero
    , negate (negate x) |.~~~~~~> x
    ]

scaleRules :: [(GuardedPattern, Pattern)]
scaleRules =
    [ x *. (y *. z) |. sameElementType [x, y] ~~~~~~> (x * y) *. z
    , negate (s *. x) |.~~~~~~> s *. negate (x)
    , xRe (s *. x) |. isReal s ~~~~~~> s *. xRe (x)
    , xIm (s *. x) |. isReal s ~~~~~~> s *. xIm (x)
    ]

-- | Rules with complex operation
--
complexNumRules :: [(GuardedPattern, Pattern)]
complexNumRules =
    [ xRe (x +: y) |.~~~~~~> x
    , xIm (x +: y) |.~~~~~~> y
    , (x +: y) + (u +: v) |.~~~~~~> (x + u) +: (y + v)
    , s *. (x +: y) |. isReal s ~~~~~~> (s *. x) +: (s *. y)
    , (x +: y) * (z +: w) |.~~~~~~> (x * z - y * w) +: (x * w + y * z)
    ]

-- | Rules with dot product and scale
--
dotProductRules :: [(GuardedPattern, Pattern)]
dotProductRules =
    [ (s *. x) <.> y |.~~~~~~> s * (x <.> y) --
    , x <.> (s *. y) |.~~~~~~> s * (x <.> y)
    ]

-- | Rules of distributive over sum
--
distributiveRules :: [(GuardedPattern, Pattern)]
distributiveRules =
    [ x * sum (each) |.~~~~~~> sum (x * each)
    , sum (each) * x |.~~~~~~> sum (x * each)
    , x <.> sum (each) |.~~~~~~> sum (x <.> each)
    , sum (each) <.> x |.~~~~~~> sum (x <.> each)
    , x *. sum (each) |.~~~~~~> sum (x *. each)
    ]

-- | Rules of piecewise
--
piecewiseRules :: [(GuardedPattern, Pattern)]
piecewiseRules =
    [ piecewise condition branches |. allTheSame branches ~~~~~~>
      headOf branches
    ]

-- | Rules of exponent and log
--
exponentRules :: [(GuardedPattern, Pattern)]
exponentRules =
    [ exp (log (x)) |.~~~~~~> x --
    , log (exp (x)) |.~~~~~~> x --
    , exp (zero) |.~~~~~~> one
    ]

-- |
--
otherRules :: [(GuardedPattern, Pattern)]
otherRules =
    [ sqrt (x * x) |.~~~~~~> x --
    ]

-- | If sum or product contains sub-sum or sub-product, flatten them out
--
reduceSumProdRules :: Simplification
reduceSumProdRules exp@(mp, n) =
    let reconstruct' :: [Int] -> (ExpressionMap, Int)
        reconstruct' = reconstruct exp . map (mp, )
     in case retrieveNode n mp of
            Sum _ ns
                -- if the sum has only one, collapse it
                -- sum(x) -> x
                | length ns == 1 -> (mp, head ns)
                -- to make sure filter (not . isZero mp) ns is not empty
                | all (isZero mp) ns -> aConst (retrieveShape n mp) 0
                -- if the sum has any zero, remove them
                -- sum(x, y, z, 0, t, 0) = sum(x, y, z, t)
                | any (isZero mp) ns ->
                    reconstruct' . filter (not . isZero mp) $ ns
                -- if the sum contains any sum, just flatten them out
                -- sum(x, sum(y, z), sum(t, u, v)) = sum(x, y, z, t, u, v)
                | otherwise ->
                    reconstruct' . concatMap (pullSumOperands mp) $ ns
            Mul _ ns
                -- if the mul has only one, collapse it
                -- product(x) -> x
                | length ns == 1 -> (mp, head ns)
                -- to make sure filter (not . isOne mp) ns is not empty
                | all (isOne mp) ns -> aConst (retrieveShape n mp) 1
                -- if the product has any one, remove them
                -- product(x, y, z, 1, t, 1) = product(x, y, z, t)
                | any (isOne mp) ns ->
                    reconstruct' . filter (not . isOne mp) $ ns
                -- if any is zero, collapse to zero
                -- product(x, y, z, 0, t, u, v) = 0
                | nId:_ <- filter (isZero mp) ns -> (mp, nId)
                -- if the prod contains any prod, just flatten them out
                -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
                | otherwise ->
                    reconstruct' . concatMap (pullProdOperands mp) $ ns
            _ -> (mp, n)

-- | If there are more than one constant in a sum or a product, group them together
--
groupConstantsRules :: Simplification
groupConstantsRules exp@(mp, n) =
    let nonConstants ns = map (mp, ) . filter (not . isConstant mp) $ ns
     in case retrieveNode n mp of
            Sum _ ns
                | Just (shape, cs) <- pullConstants mp ns
                , length cs > 1 ->
                    reconstruct exp $
                    [aConst shape $ Prelude.sum cs] ++ nonConstants ns
            Mul _ ns
                | Just (shape, cs) <- pullConstants mp ns
                , length cs > 1 ->
                    reconstruct exp $
                    [aConst shape $ Prelude.product cs] ++ nonConstants ns
            _ -> (mp, n)

-- |
--
-- Sum(x,(-1) *. x,y) -> Sum(y)
-- Sum(2 *. x, (-1) *. x,y) -> Sum(x,y)
-- Sum(x,x,y) -> Sum(2 *. x,y)
combineTermsRules :: Simplification
combineTermsRules exp@(mp, n)
    | Sum _ ns <- retrieveNode n mp =
        reconstruct exp $ map (toExp . combine) . groupBy fn . map cntAppr $ ns
    | otherwise = (mp, n)
  where
    cntAppr nId
        | Scale _ scalerN scaleeN <- retrieveNode nId mp
        , Const val <- retrieveNode scalerN mp = (scaleeN, val)
        | Neg _ negateeN <- retrieveNode nId mp = (negateeN, -1)
        | otherwise = (nId, 1)
    combine xs = (fst $ head xs, Prelude.sum $ map snd xs)
    fn x y = fst x == fst y
    toExp (nId, val)
        | val == 1 = (mp, nId)
        | val == -1 = apply (unaryET Neg ElementDefault) $ [(mp, nId)]
        | otherwise =
            apply (binaryET Scale ElementDefault) $ [aConst [] val, (mp, nId)]

-- | Remove unreachable nodes
--
removeUnreachable :: Simplification
removeUnreachable (mp, n) =
    let collectNode n =
            IS.insert n . IS.unions . map collectNode . nodeArgs $
            retrieveNode n mp
        reachableNodes = collectNode n -- Set Int
        reducedMap =
            IM.filterWithKey (\nId _ -> IS.member nId reachableNodes) mp -- Only keep those in reachable nodes
     in (reducedMap, n)

-- | Turn HashedPattern to a simplification
--
fromPattern :: (GuardedPattern, Pattern) -> Simplification
fromPattern pt@(GP pattern condition, replacementPattern) exp
    | Just match <- match exp pattern
    , condition exp match = buildFromPattern exp match replacementPattern
    | otherwise = exp

-- | Turn expression to a standard version where arguments in Sum and Mul are sorted
--
standardize :: Simplification
standardize exp@(mp, n) =
    reconstruct exp . map (mp, ) . nodeArgs $ retrieveNode n mp

-- | Turn a simplification to a recursive one, apply rules bottom up
--
makeRecursive :: Simplification -> Simplification
makeRecursive smp = recursiveSmp
  where
    recursiveSmp :: Simplification
    recursiveSmp exp@(mp, n) =
        let children = nodeArgs $ retrieveNode n mp
            simplifiedChildren = map recursiveSmp . map (mp, ) $ children
         in smp $ reconstruct exp simplifiedChildren
