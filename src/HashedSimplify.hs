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
import Data.List (foldl', group, groupBy, intercalate, sort)
import Data.List.NonEmpty (groupWith)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceShow, traceShowId)
import GHC.Exts (sortWith)
import HashedExpression
import HashedHash
import HashedInner
import HashedNode
import HashedOperation (const, const1d, const2d, const3d)
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

-- | Simplification is alias for Modification, which is (ExpressionMap, Int) -> ExpressionDiff
--
-- | Apply maximum k times, or stop if the expression doesn't change
--
multipleTimes :: Int -> Transformation -> Transformation
multipleTimes outK smp exp = go (outK - 1) exp (smp exp)
  where
    go 0 _ curExp = curExp
    go k lastExp curExp
        | snd lastExp == snd curExp = curExp
        | otherwise = go (k - 1) curExp (smp curExp)

-- | For debugging a single simplification rule
--
makeTrans ::
       (DimensionType d, ElementType et)
    => Transformation
    -> Expression d et
    -> Expression d et
makeTrans smp = wrap . smp . unwrap

-- | Simplify an expression
--
simplify ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
simplify e =
    let applyRules =
            multipleTimes 100 $
            (toRecursiveTransformation evaluateIfPossibleRules) >>>
            (toRecursiveTransformation groupConstantsRules) >>>
            (toRecursiveTransformation combineTermsRules) >>>
            (toRecursiveTransformation combineTermsRulesProd) >>>
            (toRecursiveTransformation powerProdRules) >>>
            (toRecursiveTransformation powerScaleRules) >>>
            (toRecursiveTransformation combinePowerRules) >>>
            (toRecursiveTransformation powerSumRealImagRules) >>>
            (toRecursiveTransformation combineConstantScalarRules) >>>
            (toRecursiveTransformation flattenSumProdRules) >>>
            (toRecursiveTransformation reduceSumProdRules) >>>
            rulesFromPattern >>> removeUnreachable
     in wrap . applyRules . unwrap $ e

rulesFromPattern :: Transformation
rulesFromPattern =
    chain . map (toRecursiveTransformation . fromSubstitution) . concat $
    [ complexNumRules
    , zeroOneRules
    , scaleRules
    , dotProductRules
    , distributiveRules
    , piecewiseRules
    , exponentRules
    , otherRules
    ]

-- | Rules with zero and one
--
zeroOneRules :: [Substitution]
zeroOneRules =
    [ one *. x |.~~~~~~> x
    , one * x |.~~~~~~> x
    , x * one |.~~~~~~> x
    , x ^ powerZero |.~~~~~~> one
    , x ^ powerOne |.~~~~~~> x
    , zero * x |.~~~~~~> zero
    , x * zero |.~~~~~~> zero
    , zero *. x |. isReal x ~~~~~~> zero
    , zero *. x |. isComplex x ~~~~~~> zero +: zero
    , x *. zero |.~~~~~~> zero
    , one *. x |.~~~~~~> x
    , x + zero |.~~~~~~> x
    , zero + x |.~~~~~~> x
    , x <.> zero |.~~~~~~> scalarZero
    , zero <.> x |.~~~~~~> scalarZero
    ]

scaleRules :: [Substitution]
scaleRules =
    [ x *. (y *. z) |. sameElementType [x, y] ~~~~~~> (x * y) *. z
    , negate (s *. x) |.~~~~~~> s *. negate (x)
    , xRe (s *. x) |. isReal s ~~~~~~> s *. xRe (x)
    , xIm (s *. x) |. isReal s ~~~~~~> s *. xIm (x)
    , restOfProduct ~* (s *. x) |.~~~~~~> s *. (restOfProduct ~* x)
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
    , restOfProduct ~* (x +: y) ~* (z +: w) |.~~~~~~> restOfProduct ~*
      ((x * z - y * w) +: (x * w + y * z))
    , restOfSum ~+ (x +: y) ~+ (u +: v) |.~~~~~~> restOfSum ~+
      ((x + u) +: (y + v))
    , (x +: y) *. (z +: w) |.~~~~~~> (x *. z - y *. w) +: (x *. w + y *. z)
    , (x +: y) <.> (z +: w) |.~~~~~~> (x <.> z + y <.> w) +: (y <.> z - x <.> w)
    ]

-- | Rules with dot product and scale
--
dotProductRules :: [Substitution]
dotProductRules =
    [ (s *. x) <.> y |.~~~~~~> s *. (x <.> y) --
    , x <.> (s *. y) |. isReal s ~~~~~~> s *. (x <.> y)
    , x <.> ((z +: t) *. y) |.~~~~~~> (z +: negate (t)) *. (x <.> y) -- Conjugate if the scalar is complex
    , x <.> y |. (isScalar x &&. isScalar y) &&. (isReal x &&. isReal y) ~~~~~~>
      (x * y)
    ]

-- | Rules of distributive over sum
--
distributiveRules :: [Substitution]
distributiveRules =
    [ x * sum (ys) |.~~~~~~> sum (mapL (x *) ys)
    , sum (ys) * x |.~~~~~~> sum (mapL (* x) ys)
    , x <.> sum (ys) |.~~~~~~> sum (mapL (x <.>) ys)
    , sum (ys) <.> x |.~~~~~~> sum (mapL (<.> x) ys)
    , x *. sum (ys) |.~~~~~~> sum (mapL (x *.) ys)
    , negate (sum (ys)) |.~~~~~~> sum (mapL negate ys)
    , restOfProduct ~* sum (ys) |.~~~~~~> sum (mapL (restOfProduct ~*) ys)
    , sum (ys) *. x |.~~~~~~> sum (mapL (*. x) ys)
    ]

-- | Rules of piecewise
--
piecewiseRules :: [Substitution]
piecewiseRules =
    [piecewise condition branches |. allTheSame branches ~~~~~~> headL branches]

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
    [ negate x |.~~~~~~> scalar (-1) *. x --
    , (x ^ alpha) ^ beta |.~~~~~~> x ^ (alpha * beta)
    ]

-- | 1 and 0 rules for Sum and Mul since they can involve many operands
--
reduceSumProdRules :: Modification
reduceSumProdRules exp@(mp, n) =
    case retrieveNode n mp of
        Sum _ ns
                -- if the sum has only one, collapse it
                -- sum(x) -> x
            | length ns == 1 -> noChange $ head ns
                -- to make sure filter (not . isZero mp) ns is not empty
            | all (isZero mp) ns -> noChange $ head ns
                -- if the sum has any zero, remove them
                -- sum(x, y, z, 0, t, 0) = sum(x, y, z, t)
            | any (isZero mp) ns ->
                sumManyDiff mp . map noChange . filter (not . isZero mp) $ ns
        Mul _ ns
                -- if the mul has only one, collapse it
                -- product(x) -> x
            | length ns == 1 -> noChange $ head ns
                -- to make sure filter (not . isOne mp) ns is not empty
            | all (isOne mp) ns -> noChange $ head ns
                -- if the product has any one, remove them
                -- product(x, y, z, 1, t, 1) = product(x, y, z, t)
            | any (isOne mp) ns ->
                mulManyDiff mp . map noChange . filter (not . isOne mp) $ ns
                -- if any is zero, collapse to zero
                -- product(x, y, z, 0, t, u, v) = 0
            | nId:_ <- filter (isZero mp) ns -> noChange nId
                -- if the prod contains any prod, just flatten them out
                -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
        _ -> noChange n

-- | If sum or product contains sub-sum or sub-product, flatten them out
--
flattenSumProdRules :: Modification
flattenSumProdRules exp@(mp, n) =
    case retrieveNode n mp of
        Sum _ ns
        -- if the sum contains any sum, just flatten them out
        -- sum(x, sum(y, z), sum(t, u, v)) = sum(x, y, z, t, u, v)
         -> sumManyDiff mp . map noChange . concatMap (pullSumOperands mp) $ ns
        Mul _ ns
        -- if the prod contains any prod, just flatten them out
        -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
         -> mulManyDiff mp . map noChange . concatMap (pullProdOperands mp) $ ns
        _ -> noChange n

-- | If there are more than one constant in a sum or a product, group them together
--
groupConstantsRules :: Modification
groupConstantsRules exp@(mp, n) =
    let shape = retrieveShape n mp
     in case retrieveNode n mp of
            Sum _ ns
                | Just (_, cs) <- pullConstants mp ns
                , length cs > 1
                , let diffNewConst = diffConst shape . Prelude.sum $ cs ->
                    sumManyDiff mp $
                    diffNewConst :
                    (map noChange . filter (not . isConstant mp) $ ns)
            Mul _ ns
                | Just (_, cs) <- pullConstants mp ns
                , length cs > 1
                , let diffNewConst = diffConst shape . Prelude.product $ cs ->
                    mulManyDiff mp $
                    diffNewConst :
                    (map noChange . filter (not . isConstant mp) $ ns)
            _ -> noChange n

-- |
--
-- Sum(x,(-1) *. x,y) -> Sum(y)
-- Sum(2 *. x, (-1) *. x,y) -> Sum(x,y)
-- Sum(x,x,y) -> Sum(2 *. x,y)
combineTermsRules :: Modification
combineTermsRules exp@(mp, n)
    | Sum _ ns <- retrieveNode n mp =
        sumManyDiff mp .
        map (toDiff . combine) . groupBy fn . sortWith fst . map cntAppr $
        ns
    | otherwise = noChange n
  where
    applyDiff' = applyDiff mp
    cntAppr nId
        | Scale _ scalerN scaleeN <- retrieveNode nId mp
        , Const val <- retrieveNode scalerN mp = (scaleeN, val)
        | Neg _ negateeN <- retrieveNode nId mp = (negateeN, -1)
        | otherwise = (nId, 1)
    combine xs = (fst $ head xs, Prelude.sum $ map snd xs)
    fn x y = fst x == fst y
    toDiff :: (Int, Double) -> ExpressionDiff
    toDiff (nId, val)
        | val == 1 = noChange nId
        | otherwise =
            applyDiff' (binaryET Scale ElementDefault) $
            [diffConst [] val, noChange nId]

-- |
--
-- Mul(x^(-1) * x,y) -> y
-- Mul(x,x,y) -> Mul(x^2,y), but we don't group Sum or Complex
combineTermsRulesProd :: Modification
combineTermsRulesProd exp@(mp, n)
    | Mul _ ns <- retrieveNode n mp =
        mulManyDiff mp .
        map (toDiff . combine) . groupBy fn . sortWith fst . map cntAppr $
        ns
    | otherwise = noChange n
  where
    applyDiff' = applyDiff mp
    cntAppr nId
        | Power value powerel <- retrieveNode nId mp = (powerel, value)
        | otherwise = (nId, 1)
    combine xs = (fst $ head xs, Prelude.sum $ map snd xs)
    fn (x, px) (y, py)
        | Sum _ _ <- retrieveNode x mp = False
        | Sum _ _ <- retrieveNode y mp = False
        | C <- retrieveElementType x mp = False
        | C <- retrieveElementType y mp = False
        | otherwise = x == y
    toDiff (nId, val)
        | val == 1 = noChange nId
        | otherwise = applyDiff' (unary (Power val)) [noChange nId]

-- | Rules for combining powers of power
-- (x^2)^3 -> x^6
-- (x^2)^-1 -> x^-2
combinePowerRules :: Modification
combinePowerRules exp@(mp, n)
    | Power outerVal outerN <- retrieveNode n mp
    , Power innerVal innerN <- retrieveNode outerN mp =
        applyDiff mp (unary (Power (outerVal * innerVal))) $ [noChange innerN]
    | otherwise = noChange n

-- | Rules for power of Sum and power of RealImag
-- (a+b)^2 should be (a+b)*(a+b)
-- (a +: b) ^ 2 should be (a +: b) * (a +: b)
powerSumRealImagRules :: Modification
powerSumRealImagRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , isSumOrRealImag nId = replicateMul val nId
    | otherwise = noChange n
  where
    inverse diff = applyDiff mp (unary $ Power (-1)) $ [diff]
    isSumOrRealImag nId
        | Sum _ _ <- retrieveNode nId mp = True
        | RealImag _ _ <- retrieveNode nId mp = True
        | otherwise = False
    replicateMul val nId
        | val > 1 = mulManyDiff mp . replicate val $ noChange nId
        | val < -1 =
            inverse . mulManyDiff mp . replicate (-val) . noChange $ nId
        | otherwise = noChange n

-- | Rules for power product
-- (a*b*c)^k should be a^k * b^k * c^k
powerProdRules :: Modification
powerProdRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , Mul _ args <- retrieveNode nId mp =
        let powerEach nodeId =
                applyDiff mp (unary (Power val)) [noChange nodeId]
         in mulManyDiff mp . map powerEach $ args
    | otherwise = noChange n

-- | Rules for power scale
-- (a*.b)^2 should be a^2 *. b^2
powerScaleRules :: Modification
powerScaleRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , Scale et scalar scalee <- retrieveNode nId mp
    , val > 0 =
        let powerScalar = applyDiff mp (unary (Power val)) [noChange scalar]
            powerScalee = applyDiff mp (unary (Power val)) [noChange scalee]
         in applyDiff
                mp
                (binaryET Scale (ElementSpecific et))
                [powerScalar, powerScalee]
    | otherwise = noChange n

-- | Rules for combining scale
-- ((-1) *. x) * (2 *. y) * (3 *. z) --> (-6) *. (x * y * z)
combineConstantScalarRules :: Modification
combineConstantScalarRules exp@(mp, n)
    | Mul _ ns <- retrieveNode n mp
    , let extracted = map extract ns
    , any (/= 1) . map snd $ extracted =
        let combinedConstants = Prelude.product $ map snd extracted
            combinedScalees = mulManyDiff mp . map (noChange . fst) $ extracted
         in applyDiff mp (binaryET Scale ElementDefault) $
            [diffConst [] (combinedConstants), combinedScalees]
    | otherwise = noChange n
  where
    extract nId
        | Scale _ scalar scalee <- retrieveNode nId mp
        , Const constVal <- retrieveNode scalar mp = (scalee, constVal)
        | Neg _ negateNum <- retrieveNode nId mp = (negateNum, -1)
        | otherwise = (nId, 1)

-- | Turn HashedPattern to a simplification
--
fromSubstitution :: Substitution -> Modification
fromSubstitution pt@(GP pattern condition, replacementPattern) exp@(mp, n)
    | Just match <- match exp pattern
    , condition exp match = buildFromPattern exp match replacementPattern
    | otherwise = noChange n

evaluateIfPossibleRules :: Modification
evaluateIfPossibleRules exp@(mp, n) =
    case (node, pulledVals) of
        (Sum R _, Just vals) -> res $ Prelude.sum vals
        (Mul R _, Just vals) -> res $ Prelude.product vals
        (Scale R _ _, Just [val1, val2]) -> res $ val1 * val2
        (Neg R _, Just [val])
            | val /= 0 -> res $ (-val)
            | otherwise -> res 0
        (Power x _, Just [val]) -> res $ val ^ x
        (InnerProd R arg1 arg2, Just [val1, val2]) ->
            res $
            val1 * val2 *
            (fromIntegral . Prelude.product $ retrieveShape arg1 mp)
        (Rotate _ _, Just [val]) -> res val
        -- TODO: sin, sos, ...
        _ -> noChange n
  where
    (shape, node) = retrieveInternal n mp
    getVal nId
        | Const val <- retrieveNode nId mp = Just val
        | otherwise = Nothing
    pulledVals = sequence . map getVal . nodeArgs $ node
    res val = diffConst shape val

-- | Turn expression to a standard version where arguments in Sum and Mul are sorted
--
standardize :: Transformation
standardize = toRecursiveTransformation (noChange . snd)
