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

-- | Transformation type, we can combine them, chain them, apply them n times using nest, ...
--
type Transformation = (ExpressionMap, Int) -> (ExpressionMap, Int)

-- | Simplification type, given an expression, it will give a difference (i.e, extraEntries in the ExpressionMap, and
-- the new index of the root expression) between the simplified and original expression
--
type Simplification = (ExpressionMap, Int) -> ExpressionDiff

-- | Chain n simplifications together to a simplification
--
chain :: [a -> a] -> a -> a
chain = flip $ foldl (|>)

-- |
--
applySimplification :: Simplification -> Transformation
applySimplification simp exp@(mp, n) =
    let diff = simp exp
        newMp = IM.union mp (extraEntries diff)
        newN = newRootId diff
     in (newMp, newN)

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
            (toRecursiveTransformation standardize) >>>
            rulesFromPattern >>>
            (toRecursiveTransformation groupConstantsRules) >>>
            (toRecursiveTransformation combineTermsRules) >>>
            (toRecursiveTransformation combineTermsRulesProd) >>>
            (toRecursiveTransformation powerProdRules) >>>
            (toRecursiveTransformation powerScaleRules) >>>
            (toRecursiveTransformation combinePowerRules) >>>
            (toRecursiveTransformation powerSumRealImagRules) >>>
            (toRecursiveTransformation negateRules) >>>
            (toRecursiveTransformation combineScaleRules) >>>
            (toRecursiveTransformation constPowerRules) >>>
            (toRecursiveTransformation flattenSumProdRules) >>>
            (toRecursiveTransformation reduceSumProdRules) >>> removeUnreachable
     in wrap . applyRules . unwrap $ e

toRecursiveTransformation :: Simplification -> Transformation
toRecursiveTransformation = applySimplification . makeRecursive

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
    , x <.> zero |.~~~~~~> scalarZero
    , zero <.> x |.~~~~~~> scalarZero
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

--    , x *. y |. sameElementType [x, y] &&. isScalar y &&. isNotConst x ~~~~~~> x * y -- TODO
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
    [ x * sumOf (each) |.~~~~~~> sumOf (x * each)
    , sumOf (each) * x |.~~~~~~> sumOf (x * each)
    , x <.> sumOf (each) |.~~~~~~> sumOf (x <.> each)
    , sumOf (each) <.> x |.~~~~~~> sumOf (each <.> x)
    , x *. sumOf (each) |.~~~~~~> sumOf (x *. each)
    , negate (sumOf (each)) |.~~~~~~> sumOf (negate each)
    , restOfProduct ~* sumOf (each) |.~~~~~~> sumOf (restOfProduct ~* each)
    , sumOf (each) *. x |.~~~~~~> sumOf (each *. x)
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
    case retrieveNode n mp of
        Sum _ ns
                -- if the sum has only one, collapse it
                -- sum(x) -> x
            | length ns == 1 -> withoutExtraEntry $ head ns
                -- to make sure filter (not . isZero mp) ns is not empty
            | all (isZero mp) ns -> withoutExtraEntry $ head ns
                -- if the sum has any zero, remove them
                -- sum(x, y, z, 0, t, 0) = sum(x, y, z, t)
            | any (isZero mp) ns ->
                sumManyDiff mp .
                map withoutExtraEntry . filter (not . isZero mp) $
                ns
        Mul _ ns
                -- if the mul has only one, collapse it
                -- product(x) -> x
            | length ns == 1 -> withoutExtraEntry $ head ns
                -- to make sure filter (not . isOne mp) ns is not empty
            | all (isOne mp) ns -> withoutExtraEntry $ head ns
                -- if the product has any one, remove them
                -- product(x, y, z, 1, t, 1) = product(x, y, z, t)
            | any (isOne mp) ns ->
                mulManyDiff mp . map withoutExtraEntry . filter (not . isOne mp) $
                ns
                -- if any is zero, collapse to zero
                -- product(x, y, z, 0, t, u, v) = 0
            | nId:_ <- filter (isZero mp) ns -> withoutExtraEntry nId
                -- if the prod contains any prod, just flatten them out
                -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
        _ -> withoutExtraEntry n

-- |
--
flattenSumProdRules :: Simplification
flattenSumProdRules exp@(mp, n) =
    case retrieveNode n mp of
        Sum _ ns
        -- if the sum contains any sum, just flatten them out
        -- sum(x, sum(y, z), sum(t, u, v)) = sum(x, y, z, t, u, v)
         ->
            sumManyDiff mp .
            map withoutExtraEntry . concatMap (pullSumOperands mp) $
            ns
        Mul _ ns
        -- if the prod contains any prod, just flatten them out
        -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
         ->
            mulManyDiff mp .
            map withoutExtraEntry . concatMap (pullProdOperands mp) $
            ns
        _ -> withoutExtraEntry n

-- | If there are more than one constant in a sum or a product, group them together
--
groupConstantsRules :: Simplification
groupConstantsRules exp@(mp, n) =
    let shape = retrieveShape n mp
     in case retrieveNode n mp of
            Sum _ ns
                | Just (_, cs) <- pullConstants mp ns
                , length cs > 1
                , let diffNewConst = diffConst shape . Prelude.sum $ cs ->
                    sumManyDiff mp $
                    diffNewConst :
                    (map withoutExtraEntry . filter (not . isConstant mp) $ ns)
            Mul _ ns
                | Just (_, cs) <- pullConstants mp ns
                , length cs > 1
                , let diffNewConst = diffConst shape . Prelude.product $ cs ->
                    mulManyDiff mp $
                    diffNewConst :
                    (map withoutExtraEntry . filter (not . isConstant mp) $ ns)
            _ -> withoutExtraEntry n

-- |
--
-- Sum(x,(-1) *. x,y) -> Sum(y)
-- Sum(2 *. x, (-1) *. x,y) -> Sum(x,y)
-- Sum(x,x,y) -> Sum(2 *. x,y)
combineTermsRules :: Simplification
combineTermsRules exp@(mp, n)
    | Sum _ ns <- retrieveNode n mp =
        sumManyDiff mp .
        map (toDiff . combine) . groupBy fn . sortWith fst . map cntAppr $
        ns
    | otherwise = withoutExtraEntry n
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
        | val == 1 = withoutExtraEntry nId
        | otherwise =
            applyDiff' (binaryET Scale ElementDefault) $
            [diffConst [] val, withoutExtraEntry nId]

-- |
--
-- Mul(x^(-1) * x,y) -> y
-- Mul(x,x,y) -> Mul(x^2,y), but we don't group Sum or complex
combineTermsRulesProd :: Simplification
combineTermsRulesProd exp@(mp, n)
    | Mul _ ns <- retrieveNode n mp =
        mulManyDiff mp .
        map (toDiff . combine) . groupBy fn . sortWith fst . map cntAppr $
        ns
    | otherwise = withoutExtraEntry n
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
        | val == 1 = withoutExtraEntry nId
        | otherwise = applyDiff' (unary (Power val)) [withoutExtraEntry nId]

-- | Rules for combining powers of power
-- (x^2)^3 -> x^6
-- (x^2)^-1 -> x^-2
combinePowerRules :: Simplification
combinePowerRules exp@(mp, n)
    | Power outerVal outerN <- retrieveNode n mp
    , Power innerVal innerN <- retrieveNode outerN mp =
        applyDiff mp (unary (Power (outerVal * innerVal))) $
        [withoutExtraEntry innerN]
    | otherwise = withoutExtraEntry n

-- | Rules for power of Sum and power of RealImag
-- (a+b)^2 should be (a+b)*(a+b)
-- (a +: b) ^ 2 should be (a +: b) * (a +: b)
powerSumRealImagRules :: Simplification
powerSumRealImagRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , isSumOrRealImag nId = replicateMul val nId
    | otherwise = withoutExtraEntry n
  where
    inverse diff = applyDiff mp (unary $ Power (-1)) $ [diff]
    isSumOrRealImag nId
        | Sum _ _ <- retrieveNode nId mp = True
        | RealImag _ _ <- retrieveNode nId mp = True
        | otherwise = False
    replicateMul val nId
        | val > 1 = mulManyDiff mp . replicate val $ withoutExtraEntry nId
        | val < -1 =
            inverse . mulManyDiff mp . replicate (-val) . withoutExtraEntry $
            nId
        | otherwise = withoutExtraEntry n

-- | Rules for power product
-- (a*b)^2 should be a^2 * b^2
powerProdRules :: Simplification
powerProdRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , Mul _ _ <- retrieveNode nId mp
    , val > 0 = mulManyDiff mp . replicate val . withoutExtraEntry $ nId
    | otherwise = withoutExtraEntry n

-- | Rules for power scale
-- (a*b)^2 should be a^2 * b^2
powerScaleRules :: Simplification
powerScaleRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , Scale et scalar scalee <- retrieveNode nId mp
    , val > 0 =
        let powerScalar =
                applyDiff mp (unary (Power val)) [withoutExtraEntry scalar]
            powerScalee =
                applyDiff mp (unary (Power val)) [withoutExtraEntry scalee]
         in applyDiff
                mp
                (binaryET Scale (ElementSpecific et))
                [powerScalar, powerScalee]
    | otherwise = withoutExtraEntry n

-- | Rules for constant multiplication
-- (Const val)^t ---> Const (val)^t
constPowerRules :: Simplification
constPowerRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , Const constVal <- retrieveNode nId mp
    , val > 0 = diffConst (retrieveShape n mp) (constVal ^ val)
    | otherwise = withoutExtraEntry n

-- | Rules for negate
-- if is a const:
-- -(const val) = const (-val)
-- otherwise:
-- (-x) ---> (-1) *. x
negateRules :: Simplification
negateRules exp@(mp, n)
    | Neg _ nId <- retrieveNode n mp = turnToScale nId
    | otherwise = withoutExtraEntry n
  where
    turnToScale nId
        | Const val <- retrieveNode nId mp =
            diffConst (retrieveShape nId mp) (-val)
        | otherwise =
            applyDiff
                mp
                (binaryET Scale ElementDefault)
                [diffConst [] (-1), withoutExtraEntry nId]

-- | Rules for combining scale
-- ((-1) *. x) * (2 *. y) * (3 *. z) --> (-6) *. (x * y * z)
combineScaleRules :: Simplification
combineScaleRules exp@(mp, n)
    | Mul _ ns <- retrieveNode n mp
    , let extracted = map extract ns
    , any (/= 1) . map snd $ extracted =
        let combinedConstants = Prelude.product $ map snd extracted
            combinedScalees =
                mulManyDiff mp . map (withoutExtraEntry . fst) $ extracted
         in applyDiff mp (binaryET Scale ElementDefault) $
            [diffConst [] (combinedConstants), combinedScalees]
    | otherwise = withoutExtraEntry n
  where
    extract nId
        | Scale _ scalar scalee <- retrieveNode nId mp
        , Const constVal <- retrieveNode scalar mp = (scalee, constVal)
        | Neg _ negateNum <- retrieveNode nId mp = (negateNum, -1)
        | otherwise = (nId, 1)

-- | Remove unreachable nodes
--
removeUnreachable :: Transformation
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
fromSubstitution pt@(GP pattern condition, replacementPattern) exp@(mp, n)
    | Just match <- match exp pattern
    , condition exp match = buildFromPattern exp match replacementPattern
    | otherwise = withoutExtraEntry n

checkTopo :: Transformation
checkTopo exp@(mp, n) =
    let lst = last $ topologicalSort exp
     in if (n == lst)
            then exp
            else error "Wrong topo"

-- | Turn expression to a standard version where arguments in Sum and Mul are sorted
--
standardize :: Simplification
standardize = makeRecursive (withoutExtraEntry . snd)

-- | Turn a simplification to a recursive one, apply rules bottom up
--
makeRecursive :: Simplification -> Simplification
makeRecursive smp = recursiveSmp
  where
    recursiveSmp :: Simplification
    recursiveSmp exp@(mp, n) =
        let children = nodeArgs $ retrieveNode n mp
            childrenDiffs = map recursiveSmp . map (mp, ) $ children
            nodeDiff = combineChildrenDiffs mp n childrenDiffs
            newExp = (IM.union mp $ extraEntries nodeDiff, newRootId nodeDiff)
            ExpressionDiff exEntries newId = smp newExp
         in ExpressionDiff (IM.union exEntries (extraEntries nodeDiff)) newId

-- |
--
makeRecursive1 :: Simplification -> Simplification
makeRecursive1 smp exp@(mp, n) = undefined
  where
    a = 1

-- | Same node type (Mul, Sum, Negate, ...), but children may changed, now make the same node type with new children
-- and return the combined difference
--
combineChildrenDiffs ::
       ExpressionMap -> Int -> [ExpressionDiff] -> ExpressionDiff
combineChildrenDiffs contextMp n childrenDiffs =
    let (oldShape, oldNode) = retrieveInternal n contextMp
        oldChildren = nodeArgs oldNode
        newChildren = map newRootId childrenDiffs
        combinedExtraEntries = IM.unions . map extraEntries $ childrenDiffs
        combine option =
            applyDiff contextMp (option `hasShape` oldShape) childrenDiffs
        sortAndCombine option =
            let a = 1
                getNode diff
                    | Just (_, node) <- IM.lookup (newRootId diff) contextMp =
                        node
                    | Just (_, node) <-
                         IM.lookup (newRootId diff) combinedExtraEntries = node
                nodeType diff1 diff2 =
                    sameNodeType (getNode diff1) (getNode diff2)
                weight diff = nodeTypeWeight $ getNode diff
                sortArgs =
                    concat .
                    map (sortWith newRootId) .
                    groupBy nodeType . sortWith weight
             in applyDiff contextMp (option `hasShape` oldShape) . sortArgs $
                childrenDiffs
     in if oldChildren == newChildren &&
           all (== IM.empty) (map extraEntries childrenDiffs)
            then withoutExtraEntry n
            else case oldNode of
                     Var _ -> withoutExtraEntry n
                     DVar _ -> withoutExtraEntry n
                     Const _ -> withoutExtraEntry n
                     Sum et _ ->
                         sortAndCombine (naryET Sum (ElementSpecific et))
                     Mul et _ ->
                         sortAndCombine (naryET Mul (ElementSpecific et))
                     Power x _ -> combine (unary (Power x))
                     Neg et _ -> combine (unaryET Neg (ElementSpecific et))
                     Scale et _ _ ->
                         combine (binaryET Scale (ElementSpecific et))
                     Div _ _ -> combine (binary Div)
                     Sqrt _ -> combine (unary Sqrt)
                     Sin _ -> combine (unary Sin)
                     Cos _ -> combine (unary Cos)
                     Tan _ -> combine (unary Tan)
                     Exp _ -> combine (unary Exp)
                     Log _ -> combine (unary Log)
                     Sinh _ -> combine (unary Sinh)
                     Cosh _ -> combine (unary Cosh)
                     Tanh _ -> combine (unary Tanh)
                     Asin _ -> combine (unary Asin)
                     Acos _ -> combine (unary Acos)
                     Atan _ -> combine (unary Atan)
                     Asinh _ -> combine (unary Asinh)
                     Acosh _ -> combine (unary Acosh)
                     Atanh _ -> combine (unary Atanh)
                     RealImag _ _ -> combine (binary RealImag)
                     RealPart _ -> combine (unary RealPart)
                     ImagPart _ -> combine (unary ImagPart)
                     InnerProd et _ _ ->
                         combine (binaryET InnerProd (ElementSpecific et))
                     Piecewise marks _ _ ->
                         combine (conditionAry (Piecewise marks))
                     Rotate amount _ -> combine (unary (Rotate amount))
