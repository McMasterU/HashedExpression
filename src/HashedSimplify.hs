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

multipleTimes :: Int -> Simplification -> Simplification
multipleTimes = nest

-- | Simplify an expression
--
simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e =
    let applyRules =
            (multipleTimes 10 . makeRecursive $
             scaleRules >>>
             zeroOneRules >>>
             groupConstantsRules >>>
             dotProductRules >>>
             exponentRules >>>
             combineTermsRules >>>
             complexNumRules >>> distributiveRules >>> reduceSumProdRules) >>>
            removeUnreachable
     in wrap . applyRules . unwrap $ e

-- | Rules with zero and one
--
zeroOneRules :: Simplification
zeroOneRules =
    makeRecursive . chain . map fromPattern $
    [ one *. x |.~~> x
    , one * x |.~~> x
    , x * one |.~~> x
    , zero * x |.~~> zero
    , x * zero |.~~> zero
    , zero *. x |.~~> zero
    , x *. zero |.~~> zero
    , one *. x |.~~> x
    , x + zero |.~~> x
    , zero + x |.~~> x
    , (x <.> zero) |.~~> zero
    , zero <.> x |.~~> zero
    ]

scaleRules =
    makeRecursive . chain . map fromPattern $
    [ x *. (y *. z) |.~~> (x * y) *. z
    , negate (s *. x) |.~~> s *. negate (x)
    , xRe (s *. x) |.~~> s *. xRe (x)
    , xIm (s *. x) |.~~> s *. xIm (x)
    ]

-- | Rules with complex operation
--
complexNumRules :: Simplification
complexNumRules =
    makeRecursive . chain . map fromPattern $
    [ xRe (x +: y) |.~~> x
    , xIm (x +: y) |.~~> y
    , (x +: y) + (u +: v) |.~~> (x + u) +: (y + v)
    , s *. (x +: y) |.~~> (s *. x) +: (s *. y) -- does not work for ScalarC, only vectorC; it's also in HashedComplexInstances
    , (x +: y) * (z +: w) |.~~> (x * z - y * w) +: (x * w + y * z)
    ]

-- | Rules with dot product and scale
--
dotProductRules :: Simplification
dotProductRules =
    makeRecursive . chain . map fromPattern $
    [ (s *. x) <.> y |.~~> s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
    , x <.> (s *. y) |.~~> s * (x <.> y) -- TB,CD,RF: *. --> * (FIX) 27/05/2015.
    ]

-- | Rules of distributive over sum
--
distributiveRules :: Simplification
distributiveRules =
    makeRecursive . chain . map fromPattern $
    [ x * sum (each) |.~~> sum (x * each)
    , sum (each) * x |.~~> sum (x * each)
    , x <.> sum (each) |.~~> sum (x <.> each)
    , sum (each) <.> x |.~~> sum (x <.> each)
    , x *. sum (each) |.~~> sum (x *. each)
    ]

-- | Rules of exponent and log
--
exponentRules :: Simplification
exponentRules =
    makeRecursive . chain . map fromPattern $
    [exp (log (x)) |.~~> x, log (exp (x)) |.~~> x, exp (zero) |.~~> one]

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
fromPattern pt@(GP pattern condition, replacementPattern) ex@(originalMp, originalN)
    | Just match <- match ex pattern
    , condition originalMp match =
        let (capturesMap, listCapturesMap) = match
            turnToPattern :: [Pattern -> Pattern] -> Int -> Pattern
            turnToPattern fs nId = foldr ($) (PRef nId) fs
            buildFromPatternList :: PatternList -> [(ExpressionMap, Int)]
            buildFromPatternList (PListHole fs listCapture)
                | Just ns <- Map.lookup listCapture listCapturesMap =
                    map (buildFromPattern . turnToPattern fs) ns
                | otherwise =
                    error
                        "ListCapture not in the Map ListCapture [Int] which should never happens"
            buildFromPattern :: Pattern -> (ExpressionMap, Int)
            buildFromPattern pattern =
                case pattern of
                    PRef nId -> (originalMp, nId)
                    PHole capture
                        | Just nId <- Map.lookup capture capturesMap ->
                            (originalMp, nId)
                        | otherwise ->
                            error
                                "Capture not in the Map Capture Int which should never happens"
                    PConst pc ->
                        case retrieveShape originalN originalMp of
                            [] -> unwrap $ const pc
                            [size] -> unwrap $ const1d size pc
                            [size1, size2] -> unwrap $ const2d (size1, size2) pc
                            [size1, size2, size3] ->
                                unwrap $ const3d (size1, size2, size3) pc
                            _ -> error "Dimension > 3"
                    PSumList ptl -> sumMany . buildFromPatternList $ ptl
                    PSum sps -> sumMany . map buildFromPattern $ sps
                    PMul sps -> mulMany . map buildFromPattern $ sps
                    PNeg sp ->
                        apply (unaryET Neg ElementDefault) [buildFromPattern sp]
                    PScale sp1 sp2 ->
                        apply (binaryET Scale ElementDefault) $
                        map buildFromPattern [sp1, sp2]
                    PDiv sp1 sp2 ->
                        apply (binary Div) $ map buildFromPattern [sp1, sp2]
                    PSqrt sp -> apply (unary Sqrt) [buildFromPattern sp]
                    PSin sp -> apply (unary Sin) [buildFromPattern sp]
                    PCos sp -> apply (unary Cos) [buildFromPattern sp]
                    PTan sp -> apply (unary Tan) [buildFromPattern sp]
                    PExp sp -> apply (unary Exp) [buildFromPattern sp]
                    PLog sp -> apply (unary Log) [buildFromPattern sp]
                    PSinh sp -> apply (unary Sinh) [buildFromPattern sp]
                    PCosh sp -> apply (unary Cosh) [buildFromPattern sp]
                    PTanh sp -> apply (unary Tanh) [buildFromPattern sp]
                    PAsin sp -> apply (unary Asin) [buildFromPattern sp]
                    PAcos sp -> apply (unary Acos) [buildFromPattern sp]
                    PAtan sp -> apply (unary Atan) [buildFromPattern sp]
                    PAsinh sp -> apply (unary Asinh) [buildFromPattern sp]
                    PAcosh sp -> apply (unary Acosh) [buildFromPattern sp]
                    PAtanh sp -> apply (unary Atanh) [buildFromPattern sp]
                    PRealImag sp1 sp2 ->
                        apply (binary RealImag) $
                        map buildFromPattern [sp1, sp2]
                    PRealPart sp -> apply (unary RealPart) [buildFromPattern sp]
                    PImagPart sp -> apply (unary ImagPart) [buildFromPattern sp]
                    PInnerProd sp1 sp2 ->
                        apply (binaryET InnerProd ElementDefault `hasShape` []) $
                        map buildFromPattern [sp1, sp2]
         in buildFromPattern replacementPattern
    | otherwise = (originalMp, originalN)

-- | Turn a simplification to a recursive one, that is if the rule can't apply to the root node, then apply to it's children
--
makeRecursive :: Simplification -> Simplification
makeRecursive smp exp@(mp, n)
    | newExp@(_, newN) <- smp exp
    , n /= newN = newExp
    | otherwise =
        let shape = retrieveShape n mp
            simplifiedChildren =
                map smp . map (mp, ) . nodeArgs $ retrieveNode n mp
         in reconstruct exp simplifiedChildren
