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
import Data.List (group, groupBy, intercalate)
import Data.List.NonEmpty (groupWith)
import qualified Data.Map.Strict as Map
import Debug.Trace (traceShow, traceShowId)
import GHC.Exts (sortWith)
import HashedExpression
import HashedHash
import HashedInner
import HashedNode
import HashedOperation (const, const1d, const2d, const3d, plus, times)
import HashedPattern
import HashedPrettify
import HashedUtils
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

-- | For debugging a single simplification rule
--
makeSmp ::
       (DimensionType d, ElementType et)
    => Simplification
    -> Expression d et
    -> Expression d et
makeSmp smp = wrap . smp . unwrap

-- | Simplify an expression
--
simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e =
    let applyRules =
            multipleTimes 100 $
            (makeRecursive standardize) >>>
            rulesFromPattern >>>
            (makeRecursive negateRules) >>>
            (makeRecursive reduceSumProdRules) >>>
            (makeRecursive groupConstantsRules) >>>
            (makeRecursive combineTermsRules) >>>
            (makeRecursive combineTermsRulesProd) >>>
            (makeRecursive combineScaleRules) >>>
            (makeRecursive powerProdRules) >>>
            (makeRecursive powerSumRealImagRules) >>>
            (makeRecursive combinePowerRules) >>> id
     in wrap . removeUnreachable . applyRules . unwrap $ e

rulesFromPattern :: Simplification
rulesFromPattern =
    makeRecursive . chain . map fromSubstitution $
    zeroOneRules ++
    scaleRules ++
    complexNumRules ++
    dotProductRules ++
    distributiveRules ++ piecewiseRules ++ exponentRules ++ otherRules

-- | Rules with zero and one
--
zeroOneRules :: [Substitution]
zeroOneRules =
    [ one *. x |.~~~~~~> x
    , one * x |.~~~~~~> x
    , x * one |.~~~~~~> x
    , x ^ zero |.~~~~~~> one
    , x ^ one |.~~~~~~> x
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
    , x <.> one |.~~~~~~> x
    , one <.> x |.~~~~~~> x
    , negate zero |.~~~~~~> zero
    , negate (negate x) |.~~~~~~> x
    , num (-1) *. negate (x) |.~~~~~~> x
    , num (-1) * negate (x) |.~~~~~~> x
    ]

scaleRules :: [Substitution]
scaleRules =
    [ x *. (y *. z) |. sameElementType [x, y] ~~~~~~> (x * y) *. z
    , negate (s *. x) |.~~~~~~> s *. negate (x)
    , xRe (s *. x) |. isReal s ~~~~~~> s *. xRe (x)
    , xIm (s *. x) |. isReal s ~~~~~~> s *. xIm (x)
    ]

-- | Rules with complex operation
--
complexNumRules :: [Substitution]
complexNumRules =
    [ xRe (x +: y) |.~~~~~~> x
    , xIm (x +: y) |.~~~~~~> y
    , (x +: y) + (u +: v) |.~~~~~~> (x + u) +: (y + v)
    , s *. (x +: y) |. isReal s ~~~~~~> (s *. x) +: (s *. y)
    , (x +: y) * (z +: w) |.~~~~~~> (x * z - y * w) +: (x * w + y * z)
    , negate (x +: y) |.~~~~~~> negate x +: negate y
    , (x +: y) * (zero +: zero) |.~~~~~~> zero +: zero
    , prodRest ~* (x +: y) ~* (z +: w) |.~~~~~~> prodRest ~* ((x * z - y * w) +: (x * w + y * z))
    ]

-- | Rules with dot product and scale
--
dotProductRules :: [Substitution]
dotProductRules =
    [ (s *. x) <.> y |.~~~~~~> s * (x <.> y) --
    , x <.> (s *. y) |.~~~~~~> s * (x <.> y)
    , x <.> y |. isScalar x &&. isScalar y ~~~~~~> x * y
    ]

-- | Rules of distributive over sum
--
distributiveRules :: [Substitution]
distributiveRules =
    [ x * sumOf (each) |.~~~~~~> sumOf (x * each)
    , sumOf (each) * x |.~~~~~~> sumOf (x * each)
    , x <.> sumOf (each) |.~~~~~~> sumOf (x <.> each)
    , sumOf (each) <.> x |.~~~~~~> sumOf (x <.> each)
    , x *. sumOf (each) |.~~~~~~> sumOf (x *. each)
    , negate (sumOf (each)) |.~~~~~~> sumOf (negate each)
    , prodRest ~* sumOf (each) |.~~~~~~> sumOf (prodRest ~* each)
    ]

-- | Rules of piecewise
--
piecewiseRules :: [Substitution]
piecewiseRules =
    [ piecewise condition branches |. allTheSame branches ~~~~~~>
      headOf branches
    ]

-- | Rules of exponent and log
--
exponentRules :: [Substitution]
exponentRules =
    [ exp (log (x)) |.~~~~~~> x --
    , log (exp (x)) |.~~~~~~> x --
    , exp (zero) |.~~~~~~> one
    ]

-- |
--
otherRules :: [Substitution]
otherRules =
    [ sqrt (x * x) |.~~~~~~> x --
    ]

-- | If sum or product contains sub-sum or sub-product, flatten them out
--
reduceSumProdRules :: Simplification
reduceSumProdRules exp@(mp, n) =
    let reconstruct' :: [Int] -> (ExpressionMap, Int)
        reconstruct' = reconstruct exp . map (mp, )
        properZero = aConst (retrieveShape n mp) 0
        properOne = aConst (retrieveShape n mp) 1
        elementType = retrieveElementType n mp
     in case retrieveNode n mp of
            Sum _ ns
                -- if the sum has only one, collapse it
                -- sum(x) -> x
                | length ns == 1 -> (mp, head ns)
                -- to make sure filter (not . isZero mp) ns is not empty
                | all (isZero mp) ns ->
                    if elementType == C
                        then apply (binary RealImag) [properZero, properZero]
                        else properZero
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
                | all (isOne mp) ns ->
                    if elementType == C
                        then apply (binary RealImag) [properOne, properZero]
                        else properOne
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
        reconstruct exp $
        map (toExp . combine) . groupBy fn . sortWith fst . map cntAppr $ ns
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
        | otherwise =
            apply (binaryET Scale ElementDefault) $ [aConst [] val, (mp, nId)]

-- |
--
-- Mul(x^(-1) * x,y) -> y
-- Mul(x,x,y) -> Mul(x^2,y), but we don't group Sum or RealImag
combineTermsRulesProd :: Simplification
combineTermsRulesProd exp@(mp, n)
    | Mul _ ns <- retrieveNode n mp =
        reconstruct exp $
        map (toExp . combine) . groupBy fn . sortWith fst . map cntAppr $ ns
    | otherwise = (mp, n)
  where
    cntAppr nId
        | Power value powerel <- retrieveNode nId mp = (powerel, value)
        | otherwise = (nId, 1)
    combine xs = (fst $ head xs, Prelude.sum $ map snd xs)
    fn (x, px) (y, py)
        | Sum _ _ <- retrieveNode x mp = False
        | Sum _ _ <- retrieveNode y mp = False
        | RealImag _ _ <- retrieveNode x mp = False
        | RealImag _ _ <- retrieveNode y mp = False
        | otherwise = x == y
    toExp (nId, val)
        | val == 1 = (mp, nId)
        | otherwise = apply (unary (Power val)) $ [(mp, nId)]

-- | Rules for combining powers of power
-- (x^2)^3 -> x^6
-- (x^2)^-1 -> x^-2
combinePowerRules :: Simplification
combinePowerRules exp@(mp, n)
    | Power powerVal wholeExpr <- retrieveNode n mp
    , Power powerVal1 innerExpr <- retrieveNode wholeExpr mp =
        apply (unary (Power (powerVal * powerVal1))) $ [(mp, innerExpr)]
    | otherwise = exp

-- | Rules for power of Sum and power of RealImag
-- (a+b)^2 should be (a+b)*(a+b)
-- (a +: b) ^ 2 should be (a +: b) * (a +: b)
powerSumRealImagRules :: Simplification
powerSumRealImagRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , isSumOrRealImag nId =
        if val > 1
            then mulMany $ replicate val (mp, nId)
            else if val < -1
                     then inverse . mulMany $ replicate (-val) (mp, nId)
                     else exp
    | otherwise = exp
  where
    inverse e = apply (unary $ Power (-1)) $ [e]
    isSumOrRealImag nId
        | Sum _ _ <- retrieveNode nId mp = True
        | RealImag _ _ <- retrieveNode nId mp = True
        | otherwise = False

-- | Rules for power product
-- (a*b)^2 should be a^2 * b^2
powerProdRules :: Simplification
powerProdRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , Mul _ _ <- retrieveNode nId mp
    , val > 0 = mulMany $ replicate val (mp, nId)
    | otherwise = exp

-- | Rules for negate
-- (-x) ---> (-1) *. x
negateRules :: Simplification
negateRules exp@(mp, n)
    | Neg _ nId <- retrieveNode n mp =
        case retrieveNode nId mp of
            Const _ -> exp
            _ ->
                apply (binaryET Scale ElementDefault) $
                [aConst [] (-1), (mp, nId)]
    | otherwise = exp

-- | Rules for combining scale
-- ((-1) *. x) * (2 *. y) * (3 *. z) ---> (-6) *. (x * y * z)
combineScaleRules :: Simplification
combineScaleRules exp@(mp, n)
    | Mul _ ns <- retrieveNode n mp
    , let extracted = map extract ns
    , any (/= 1) . map snd $ extracted =
        let combinedConstants = Prelude.product $ map snd extracted
            combinedScalees = mulMany . map (mp, ) . map fst $ extracted
         in apply (binaryET Scale ElementDefault) $
            [aConst [] (combinedConstants), combinedScalees]
    | otherwise = exp
  where
    extract nId
        | Scale _ scalar scalee <- retrieveNode nId mp
        , Const constVal <- retrieveNode scalar mp = (scalee, constVal)
        | Neg _ negateNum <- retrieveNode nId mp = (negateNum, -1)
        | otherwise = (nId, 1)

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
fromSubstitution :: Substitution -> Simplification
fromSubstitution pt@(GP pattern condition, replacementPattern) exp
    | Just match <- match exp pattern
    , condition exp match = buildFromPattern exp match replacementPattern
    | otherwise = exp

-- | Turn expression to a standard version where arguments in Sum and Mul are sorted
--
standardize :: Simplification
standardize = makeRecursive id

--    reconstruct exp . map (mp, ) . nodeArgs $ retrieveNode n mp
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

-- | Reconstruct
--
reconstruct ::
       (ExpressionMap, Int) -> [(ExpressionMap, Int)] -> (ExpressionMap, Int)
reconstruct oldExp@(oldMp, oldN) newChildren =
    let (oldShape, oldNode) = retrieveInternal oldN oldMp
        apply' option = apply (option `hasShape` oldShape) -- keep the old shape
     in case oldNode of
            Var _ -> oldExp
            DVar _ -> oldExp
            Const _ -> oldExp
            Sum et _ ->
                apply' (naryET Sum (ElementSpecific et)) $ sortArgs newChildren
            Mul et _ ->
                apply' (naryET Mul (ElementSpecific et)) $ sortArgs newChildren
            Power x _ -> apply' (unary (Power x)) newChildren
            Neg et _ -> apply' (unaryET Neg (ElementSpecific et)) newChildren
            Scale et _ _ ->
                apply' (binaryET Scale (ElementSpecific et)) newChildren
            Div _ _ -> apply' (binary Div) newChildren
            Sqrt _ -> apply' (unary Sqrt) newChildren
            Sin _ -> apply' (unary Sin) newChildren
            Cos _ -> apply' (unary Cos) newChildren
            Tan _ -> apply' (unary Tan) newChildren
            Exp _ -> apply' (unary Exp) newChildren
            Log _ -> apply' (unary Log) newChildren
            Sinh _ -> apply' (unary Sinh) newChildren
            Cosh _ -> apply' (unary Cosh) newChildren
            Tanh _ -> apply' (unary Tanh) newChildren
            Asin _ -> apply' (unary Asin) newChildren
            Acos _ -> apply' (unary Acos) newChildren
            Atan _ -> apply' (unary Atan) newChildren
            Asinh _ -> apply' (unary Asinh) newChildren
            Acosh _ -> apply' (unary Acosh) newChildren
            Atanh _ -> apply' (unary Atanh) newChildren
            RealImag _ _ -> apply' (binary RealImag) newChildren
            RealPart _ -> apply' (unary RealPart) newChildren
            ImagPart _ -> apply' (unary ImagPart) newChildren
            InnerProd et _ _ ->
                apply' (binaryET InnerProd (ElementSpecific et)) newChildren
            Piecewise marks _ _ ->
                apply (conditionAry (Piecewise marks)) newChildren
            Rotate amount _ -> apply (unary (Rotate amount)) newChildren

-- | Sort the arguments (now only for Sum and Mul)
--
sortArgs :: [(ExpressionMap, Int)] -> [(ExpressionMap, Int)]
sortArgs = concat . map (sortWith snd) . groupBy nodeType . sortWith weight
  where
    nodeType (mp1, n1) (mp2, n2) =
        sameNodeType (retrieveNode n1 mp1) (retrieveNode n2 mp2)
    weight (mp, n) = nodeTypeWeight $ retrieveNode n mp
