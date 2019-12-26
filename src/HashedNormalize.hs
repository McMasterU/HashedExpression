-------------------------------------------------------------------------------
-- | For normalizeing expressions
--
-------------------------------------------------------------------------------
module HashedNormalize where

import Data.Eq.HT (equating)
import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
    ( find
    , foldl'
    , group
    , groupBy
    , intercalate
    , partition
    , sort
    , sortBy
    , sortOn
    , transpose
    )
import Data.List.Extra (firstJust, groupSort)
import Data.List.NonEmpty (groupWith)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, mapMaybe)
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

-- | For debugging a single normalizier rule
--
makeTrans ::
       (DimensionType d, ElementType et)
    => Transformation
    -> Expression d et
    -> Expression d et
makeTrans smp = wrap . smp . unwrap

-- | Normalize an expression
--
normalize ::
       (DimensionType d, ElementType et) => Expression d et -> Expression d et
normalize = wrap . normalizingTransformation . unwrap

-- | Combine all the transformations
--
normalizingTransformation :: Transformation
normalizingTransformation = secondPass . firstPass
  where
    firstPass =
        multipleTimes 100 . chain $
        map
            toRecursiveSimplification
            [ helper evaluateIfPossibleRules
            , helper groupConstantsRules
            , helper combineTermsRules
            , helper combineTermsRulesProd
            , helper powerProdRules
            , helper powerScaleRules
            , helper combinePowerRules
            , helper powerSumRealImagRules
            , helper combineRealScalarRules
            , helper flattenSumProdRules
            , helper zeroOneSumProdRules
            , helper collapseSumProdRules
            , helper normalizeRotateRules
            , helper negativeZeroRules
            
            , pullOutPiecewiseRules
            , expandPiecewiseRealImag
            , twiceReFTAndImFTRules
            ] ++
        [ rulesFromPattern --
        , removeUnreachable
        ]
    secondPass = toMultiplyIfPossible

helper :: Modification1 -> Modification
helper mkDiff exp@(mp, n) = mkDiff exp mp

-- | Turn a modification to a recursive transformation
--
toRecursiveSimplification :: Modification -> Transformation
toRecursiveSimplification = toTransformation . toRecursiveModification Reorder

-- | Turn to multiplication if possible (i.e, scale a scalar R or Covector,
-- inner product between 2 scalar R or Covector) (this is performed after all other rules completed)
--
toMultiplyIfPossible :: Transformation
toMultiplyIfPossible = toRecursiveSimplification rule
  where
    rule exp@(mp, n)
        | Scale et scalar scalee <- retrieveNode n mp
        , et /= C
        , isScalarShape (retrieveShape scalee mp) =
            mulManyDiff mp [noChange scalar, noChange scalee]
        | InnerProd et arg1 arg2 <- retrieveNode n mp
        , isScalarShape (retrieveShape arg1 mp)
        , isScalarShape (retrieveShape arg2 mp)
        , et /= C = mulManyDiff mp [noChange arg1, noChange arg2]
        | otherwise = noChange n

-- | Equivalent version of toMultiplyIfPossible, but slower. Though I think it doesn't really matter which one we choose.
--
toMultiplyIfPossible1 :: Transformation
toMultiplyIfPossible1 =
    chain . map (toRecursiveSimplification . fromSubstitution) $
    [ x *. y |. isReal y &&. isScalar y ~~~~~~> x * y
    , x *. y |. isCovector y &&. isScalar y ~~~~~~> x * y
    , x <.> y |. isReal y &&. isScalar y &&. isScalar x ~~~~~~> x * y
    , x <.> y |. isReal x &&. isScalar x &&. isScalar y ~~~~~~> x * y
    , x <.> y |. isCovector y &&. isScalar y &&. isScalar x ~~~~~~> x * y
    , x <.> y |. isCovector x &&. isScalar x &&. isScalar y ~~~~~~> x * y
    ]

rulesFromPattern :: Transformation
rulesFromPattern =
    chain . map (toRecursiveSimplification . fromSubstitution) . concat $
    [ complexNumRules
    , zeroOneRules
    , scaleRules
    , dotProductRules
    , distributiveRules
    , piecewiseRules
    , exponentRules
    , rotateRules
    , fourierTransformRules
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
    , negate (s *. x) |.~~~~~~> s *. negate x
    , xRe (s *. x) |. isReal s ~~~~~~> s *. xRe x
    , xIm (s *. x) |. isReal s ~~~~~~> s *. xIm x
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
    , x <.> (s *. y) |. isCovector s ~~~~~~> s *. (x <.> y)
    , x <.> ((z +: t) *. y) |.~~~~~~> (z +: negate t) *. (x <.> y) -- Conjugate if the scalar is complex
    , x <.> y |. (isScalar x &&. isScalar y) &&. (isReal x &&. isReal y) ~~~~~~>
      (x * y)
    ]

-- | Rules of distributive over sum
--
distributiveRules :: [Substitution]
distributiveRules =
    [ x * sum ys |.~~~~~~> sum (mapL (x *) ys)
    , sum ys * x |.~~~~~~> sum (mapL (* x) ys)
    , x <.> sum ys |.~~~~~~> sum (mapL (x <.>) ys)
    , sum ys <.> x |.~~~~~~> sum (mapL (<.> x) ys)
    , x *. sum ys |.~~~~~~> sum (mapL (x *.) ys)
    , negate (sum ys) |.~~~~~~> sum (mapL negate ys)
    , restOfProduct ~* sum ys |.~~~~~~> sum (mapL (restOfProduct ~*) ys)
    , sum ys *. x |.~~~~~~> sum (mapL (*. x) ys)
    ]

-- | Fourier transform rules
--
fourierTransformRules :: [Substitution]
fourierTransformRules =
    [ reFT (x +: y) |.~~~~~~> reFT x - imFT y
    , imFT (x +: y) |.~~~~~~> imFT x + reFT y
    , reFT zero |.~~~~~~> zero
    , imFT zero |.~~~~~~> zero
    , twiceImFT zero |.~~~~~~> zero
    , twiceReFT zero |.~~~~~~> zero
    , reFT (sum xs) |.~~~~~~> sum (mapL reFT xs)
    , imFT (sum xs) |.~~~~~~> sum (mapL imFT xs)
    , imFT (reFT x) |. isReal x ~~~~~~> zero
    , reFT (imFT x) |. isReal x ~~~~~~> zero
    , imFT (reFT x) |. isCovector x ~~~~~~> zero
    , reFT (imFT x) |. isCovector x ~~~~~~> zero
    , reFT (s *. x) |. isReal s ~~~~~~> s *. reFT x
    , reFT (s *. x) |. isCovector s ~~~~~~> s *. reFT x
    , imFT (s *. x) |. isReal s ~~~~~~> s *. imFT x
    , imFT (s *. x) |. isCovector s ~~~~~~> s *. imFT x
    , reFT (reFT x) |. isReal x ~~~~~~> twiceReFT x
    , imFT (imFT x) |. isReal x ~~~~~~> twiceImFT x
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
    [ exp (log x) |.~~~~~~> x --
    , log (exp x) |.~~~~~~> x --
    , exp zero |.~~~~~~> one
    ]

-- |
--
otherRules :: [Substitution]
otherRules =
    [ negate x |.~~~~~~> scalar (-1) *. x --
    , (x ^ alpha) ^ beta |.~~~~~~> x ^ (alpha * beta)
    ]

-- |
--
rotateRules :: [Substitution]
rotateRules =
    [ rotate amount (s *. x) |.~~~~~~> s *. rotate amount x
    , rotate amount1 (rotate amount2 x) |.~~~~~~> rotate (amount1 + amount2) x
    , rotate amount x |. zeroAmount amount ~~~~~~> x
    , rotate amount (sum xs) |.~~~~~~> sum (mapL (rotate amount) xs)
    , rotate amount (product xs) |.~~~~~~> product (mapL (rotate amount) xs)
    , rotate amount (x +: y) |.~~~~~~> rotate amount x +: rotate amount y
    , rotate amount1 x <.> rotate amount2 y |. sameAmount amount1 amount2 ~~~~~~>
      (x <.> y)
    ]

-- | 1 and 0 rules for Sum and Mul since they can involve many operands
--
zeroOneSumProdRules :: Modification1
zeroOneSumProdRules exp@(mp, n) =
    case retrieveNode n mp of
        Sum _ ns
                -- to make sure filter (not . isZero mp) ns is not empty
            | all (isZero mp) ns -> just $ head ns
                -- if the sum has any zero, remove them
                -- sum(x, y, z, 0, t, 0) = sum(x, y, z, t)
            | any (isZero mp) ns ->
                sum_ . map just . filter (not . isZero mp) $ ns
        Mul _ ns
                -- to make sure filter (not . isOne mp) ns is not empty
            | all (isOne mp) ns -> just $ head ns
                -- if the product has any one, remove them
                -- product(x, y, z, 1, t, 1) = product(x, y, z, t)
            | any (isOne mp) ns ->
                product_ . map just . filter (not . isOne mp) $ ns
                -- if any is zero, collapse to zero
                -- product(x, y, z, 0, t, u, v) = 0
            | nId:_ <- filter (isZero mp) ns -> just nId
                -- if the prod contains any prod, just flatten them out
                -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
        _ -> just n

-- |
--
collapseSumProdRules :: Modification1
collapseSumProdRules exp@(mp, n) =
    case retrieveNode n mp of
        Sum _ ns
                -- if the sum has only one, collapse it
                -- sum(x) -> x
            | length ns == 1 -> just $ head ns
        Mul _ ns
                -- if the mul has only one, collapse it
                -- product(x) -> x
            | length ns == 1 -> just $ head ns
        _ -> just n

-- | If sum or product contains sub-sum or sub-product, flatten them out
--
flattenSumProdRules :: Modification1
flattenSumProdRules exp@(mp, n) =
    case retrieveNode n mp of
        Sum _ ns
        -- if the sum contains any sum, just flatten them out
        -- sum(x, sum(y, z), sum(t, u, v)) = sum(x, y, z, t, u, v)
         -> sum_ . map just . concatMap (pullSumOperands mp) $ ns
        Mul _ ns
        -- if the prod contains any prod, just flatten them out
        -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
         -> product_ . map just . concatMap (pullProdOperands mp) $ ns
        _ -> just n

-- | If there are more than one constant in a sum or a product, group them together
--
groupConstantsRules :: Modification1
groupConstantsRules exp@(mp, n) =
    let shape = retrieveShape n mp
     in case retrieveNode n mp of
            Sum _ ns
                | Just (_, cs) <- pullConstants mp ns
                , length cs > 1
                , let total = const_ shape . Prelude.sum $ cs ->
                    sum_ $
                    total : (map just . filter (not . isConstant mp) $ ns)
            Mul _ ns
                | Just (_, cs) <- pullConstants mp ns
                , length cs > 1
                , let total = const_ shape . Prelude.product $ cs ->
                    product_ $
                    total : (map just . filter (not . isConstant mp) $ ns)
            _ -> just n

-- |
--
-- Sum(x,(-1) *. x,y) -> Sum(y)
-- Sum(2 *. x, (-1) *. x,y) -> Sum(x,y)
-- Sum(x,x,y) -> Sum(2 *. x,y)
combineTermsRules :: Modification1
combineTermsRules exp@(mp, n)
    | Sum _ ns <- retrieveNode n mp =
        sum_ . map (toDiff . combine) . groupBy fn . sortWith fst . map cntAppr $
        ns
    | otherwise = just n
  where
    applyDiff' = applyDiff mp
    cntAppr nId
        | Scale _ scalerN scaleeN <- retrieveNode nId mp
        , Const val <- retrieveNode scalerN mp = (scaleeN, val)
        | Neg _ negateeN <- retrieveNode nId mp = (negateeN, -1)
        | otherwise = (nId, 1)
    combine xs = (fst $ head xs, Prelude.sum $ map snd xs)
    fn x y = fst x == fst y
    toDiff :: (Int, Double) -> MakeDiff
    toDiff (nId, val)
        | val == 1 = just nId
        | otherwise = const_ [] val *. just nId

-- |
--
-- Mul(x^(-1) * x,y) -> y
-- Mul(x,x,y) -> Mul(x^2,y), but we don't group Sum or Complex
combineTermsRulesProd :: Modification1
combineTermsRulesProd exp@(mp, n)
    | Mul _ ns <- retrieveNode n mp =
        product_ .
        map (toDiff . combine) . groupBy fn . sortWith fst . map cntAppr $
        ns
    | otherwise = just n
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
        | val == 1 = just nId
        | otherwise = just nId ^ val

-- | Rules for combining powers of power
-- (x^2)^3 -> x^6
-- (x^2)^-1 -> x^-2
combinePowerRules :: Modification1
combinePowerRules exp@(mp, n)
    | Power outerVal outerN <- retrieveNode n mp
    , Power innerVal innerN <- retrieveNode outerN mp =
        just innerN ^ (outerVal * innerVal)
    | otherwise = just n

-- | Rules for power of Sum and power of RealImag
-- (a+b)^2 should be (a+b)*(a+b)
-- (a +: b) ^ 2 should be (a +: b) * (a +: b)
powerSumRealImagRules :: Modification1
powerSumRealImagRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , isSumOrRealImag nId = replicateMul val nId
    | otherwise = just n
  where
    isSumOrRealImag nId
        | Sum _ _ <- retrieveNode nId mp = True
        | RealImag _ _ <- retrieveNode nId mp = True
        | otherwise = False
    replicateMul val nId
        | val > 1 = product_ . replicate val $ just nId
        | val < -1 = (^ (-1)) . product_ . replicate (-val) . just $ nId
        | otherwise = just nId

-- | Rules for power product
-- (a*b*c)^k should be a^k * b^k * c^k
powerProdRules :: Modification1
powerProdRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , Mul _ args <- retrieveNode nId mp = product_ . map ((^ val) . just) $ args
    | otherwise = just n

-- | Rules for power scale
-- (a*.b)^2 should be a^2 *. b^2
powerScaleRules :: Modification1
powerScaleRules exp@(mp, n)
    | Power val nId <- retrieveNode n mp
    , Scale et scalar scalee <- retrieveNode nId mp
    , val > 0 = (just scalar ^ val) *. (just scalee ^ val)
    | otherwise = just n

-- | Rules for combining scalar
-- (a *. x) * (b *. y) * (c *. z) --> (a * b * c) *. (x * y * z) (if all are real)
--
combineRealScalarRules :: Modification1
combineRealScalarRules exp@(mp, n)
    | Mul R ns <- retrieveNode n mp
    , let extracted = map extract ns
    , any isJust . map snd $ extracted =
        let combinedScalars = product_ . catMaybes $ map snd extracted
            combinedScalees = product_ $ map fst extracted
         in combinedScalars *. combinedScalees
    | otherwise = just n
  where
    extract nId
        | Scale _ scalar scalee <- retrieveNode nId mp =
            (just scalee, Just $ just scalar)
        | Neg R negatee <- retrieveNode nId mp =
            (just negatee, Just $ const_ [] (-1))
        | otherwise = (just nId, Nothing)

-- |
--
evaluateIfPossibleRules :: Modification1
evaluateIfPossibleRules exp@(mp, n) =
    case (node, pulledVals) of
        (Sum R _, Just vals) -> res $ Prelude.sum vals
        (Mul R _, Just vals) -> res $ Prelude.product vals
        (Scale R _ _, Just [val1, val2]) -> res $ val1 * val2
        (Neg R _, Just [val])
            | val /= 0 -> res (-val)
            | otherwise -> res 0
        (Power x _, Just [val]) -> res $ val ** fromIntegral x
        (InnerProd R arg1 arg2, Just [val1, val2]) ->
            res $
            val1 * val2 *
            (fromIntegral . Prelude.product $ retrieveShape arg1 mp)
        (Rotate _ _, Just [val]) -> res val
        -- TODO: sin, sos, ...
        _ -> just n
  where
    (shape, node) = retrieveInternal n mp
    getVal nId
        | Const val <- retrieveNode nId mp = Just val
        | otherwise = Nothing
    pulledVals = mapM getVal . nodeArgs $ node
    res = const_ shape

-- | rotateAmount in each direction always lie within (0, dim - 1)
--
normalizeRotateRules :: Modification1
normalizeRotateRules exp@(mp, n)
    | (shape, Rotate amount arg) <- retrieveInternal n mp =
        rotate (zipWith mod amount shape) $ just arg
    | otherwise = just n

-- | Because sometimes we got both Const (-0.0) and Const (0.0) which are different, so we turn them
-- to Const (0.0)
--
negativeZeroRules :: Modification1
negativeZeroRules exp@(mp, n)
    | (shape, Const val) <- retrieveInternal n mp
    , val == 0.0 || val == (-0.0) = const_ shape 0
    | otherwise = just n

-- | Piecewise of RealImag -> RealImag of piecewises
-- if a > 2 then x +: y else m +: n --> (if a > 2 then x else m) +: (if a > 2 then y else n)
--
expandPiecewiseRealImag :: Modification
expandPiecewiseRealImag exp@(mp, n)
    | Piecewise marks condition branches <- retrieveNode n mp
    , Just reIms <- mapM extract branches =
        let (res, ims) = unzip reIms
            rePart =
                applyDiff mp (conditionAry (Piecewise marks)) . map noChange $
                condition : res
            imPart =
                applyDiff mp (conditionAry (Piecewise marks)) . map noChange $
                condition : ims
         in applyDiff mp (binary RealImag) [rePart, imPart]
    | otherwise = noChange n
  where
    extract nId
        | RealImag re im <- retrieveNode nId mp = Just (re, im)
        | otherwise = Nothing

-- | piecewise marks condition [branch1, branch2, ..] -->
-- branch1 * piecewise marks condition [1, 0, 0, ..] + branch2 * piecewise marks condition [0, 1, 0, ..] + ..
--
pullOutPiecewiseRules :: Modification
pullOutPiecewiseRules exp@(mp, n)
    | Piecewise marks condition branches <- retrieveNode n mp
    , et /= C
    , not (isAllZeroExceptOne branches) =
        let branchWithPiecewise (idx, branch) =
                let newPiecewiseBranches -- [1, 0, 0 .. ]
                     =
                        replicate idx zero ++
                        [one] ++ replicate (length branches - idx - 1) zero
                    allZerosOneOne -- piecewise marks condition [1, 0, 0, ..]
                     =
                        applyDiff mp (conditionAry (Piecewise marks)) $
                        noChange condition : newPiecewiseBranches
                 in mulManyDiff mp [noChange branch, allZerosOneOne]
         in sumManyDiff mp . zipWith (curry branchWithPiecewise) [0 ..] $
            branches
    | otherwise = noChange n
  where
    isAllZeroExceptOne branches
        | [shouldBeOne] <- filter (not . isZero mp) branches
        , isOne mp shouldBeOne = True
        | otherwise = False
    et = retrieveElementType n mp
    shape = retrieveShape n mp
    one = diffConst shape 1
    zero = diffConst shape 0

-- | (s *. ) twiceReFT(x) + (s *. ) twiceImFT(x) ~~~> (s * size(x) / 2) *. x
--
twiceReFTAndImFTRules :: Modification
twiceReFTAndImFTRules exp@(mp, n)
    | Sum R args <- retrieveNode n mp
    , Just (scaleFactor, twiceReFTid, innerArg) <- firstJust isTwiceReFT args
    , Just twiceImFTid <- find (isTwiceImFTof innerArg scaleFactor) args =
        let rest =
                map noChange .
                filter (\x -> x /= twiceReFTid && x /= twiceImFTid) $
                args
            totalScaleFactor =
                scaleFactor *
                fromIntegral (Prelude.product $ retrieveShape innerArg mp)
            scalar = diffConst [] totalScaleFactor
            scaled =
                applyDiff
                    mp
                    (binaryET Scale ElementDefault)
                    [scalar, noChange innerArg]
         in sumManyDiff mp $ scaled : rest
    | otherwise = noChange n
  where
    isTwiceReFT nId
        | TwiceReFT inner <- retrieveNode nId mp = Just (1, nId, inner)
        | Scale R scalarId scaleeId <- retrieveNode nId mp
        , Const val <- retrieveNode scalarId mp
        , TwiceReFT inner <- retrieveNode scaleeId mp = Just (val, nId, inner)
        | otherwise = Nothing
    isTwiceImFTof innerArg scaleFactor nId
        | TwiceImFT x <- retrieveNode nId mp
        , scaleFactor == 1
        , x == innerArg = True
        | Scale R scalarId scaleeId <- retrieveNode nId mp
        , Const val <- retrieveNode scalarId mp
        , val == scaleFactor
        , TwiceImFT x <- retrieveNode scaleeId mp
        , x == innerArg = True
        | otherwise = False
