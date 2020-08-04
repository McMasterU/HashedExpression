-- |
-- Module      :  HashedExpression.Internal.Normalize
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module contains functionality for normalizing an 'Expression' using a variety of rewrite rules designed primarily to simplify and
-- reduce computation when evaluating. Currently there is no proof of confluence, however confluence is suspected
--
-- TODO haddock: should we summerize rewrite rules here???
module HashedExpression.Internal.Normalize
  ( normalize,
    normalizingTransformation,
    flattenSumProdRules,
    toMultiplyIfPossible,
  )
where

import Data.Eq.HT (equating)
import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
  ( find,
    foldl',
    group,
    groupBy,
    intercalate,
    partition,
    sort,
    sortBy,
    sortOn,
    transpose,
  )
import Data.List.Extra (firstJust, groupSort)
import Data.List.NonEmpty (groupWith)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, mapMaybe)
import Debug.Trace (traceShow, traceShowId)
import GHC.Exts (sortWith)
import HashedExpression.Internal.Context hiding (imFT, reFT)
import HashedExpression.Internal hiding (const_, just, num_, product_, sum_)
import HashedExpression.Internal.Rewrite
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.Pattern
import HashedExpression.Internal.Utils
import HashedExpression.Operation (constant)
import HashedExpression.Prettify
import Prelude hiding (product, sum, (^))
import qualified Prelude
import Control.Monad.State.Strict

-- | Predefined holes used for pattern matching with 'Pattern'
[p, q, r, s, t, u, v, w, x, y, z, condition] = map PHole [1 .. 12]

[dx, dy, dz] = map PHole [20, 21, 22]

zero :: Pattern
zero = PConst 0.0

one :: Pattern
one = PConst 1.0

-- | Predefined holes used for pattern matching with 'PPowerHole'
[alpha, beta, gamma] = map PPowerHole [1 .. 3]

-- | Predefined holes used for pattern matching with 'PRotateAmountHole'
[amount, amount1, amount2, amount3] = map PRotateAmountHole [1 .. 4]

-- | Predefined holes used for pattern matching with 'PListHole'
[xs, ys] = map (PListHole id) [1, 2]

[dxs, dys] = map (PListHole id) [3, 4]

-- | Constant pattern x ^ 1
powerOne :: PatternPower
powerOne = PPowerConst 1

-- | Constant pattern x ^ 0
powerZero :: PatternPower
powerZero = PPowerConst 0

-- | For debugging a single normalizier rule
makeTrans ::
  (DimensionType d, ElementType et) =>
  Transformation ->
  Expression d et ->
  Expression d et
makeTrans smp = wrap . smp . unwrap

-- | Normalize an expression considering all possible rewrite rules in this module
normalize ::
  (DimensionType d, ElementType et) =>
  Expression d et -> -- un-normalized expression
  Expression d et -- normalized expression
normalize = wrap . normalizingTransformation . unwrap

-- | Combine all the transformations, using a two-pass normalization procedure
--   Pass one transforms the expression a maximum of 100 times, or until it does not change anymore
--   Pass two attempts to turn the expression into a top-level multiplication if possible (see `toMultiplyIfPossible`)
--   See Internal.Inner.multipleTimes for greater control over this
normalizingTransformation :: Transformation
normalizingTransformation = removeUnreachable . pass . toMultiplyIfPossible . pass
  where
    pass =
      multipleTimes 1000 . chain $
        map
          toRecursiveTransformation
          [ reorderOperands,
            evaluateIfPossibleRules,
            groupConstantsRules,
            combineTermsRules,
            combineTermsRulesProd,
            powerProdRules,
            powerScaleRules,
            combinePowerRules,
            powerSumRealImagRules,
            combineRealScalarRules,
            flattenSumProdRules,
            zeroOneSumProdRules,
            collapseSumProdRules,
            normalizeRotateRules,
            negativeZeroRules,
            pullOutPiecewiseRules,
            expandPiecewiseRealImag,
            twiceReFTAndImFTRules
          ]
          ++ [rulesFromPattern]


-- | Equivalent version of toMultiplyIfPossible, but slower. Though I think it doesn't really matter which one we choose.
toMultiplyIfPossible :: Transformation
toMultiplyIfPossible =
  chain . map (toRecursiveTransformation . fromSubstitution) $
    [ x *. y |. isReal y &&. isScalar y ~~~~~~> x * y,
      x <.> y |. isReal y &&. isScalar y &&. isScalar x ~~~~~~> x * y,
      x <.> y |. isReal x &&. isScalar x &&. isScalar y ~~~~~~> x * y,
      x |*.| dy |. isScalar dy ~~~~~~> x |*| dy,
      x |<.>| dy |. isScalar x &&. isScalar dy ~~~~~~> x |*| dy
    ]

-- | Create a transformation which combines together the various rules listed in this module.
rulesFromPattern :: Transformation
rulesFromPattern =
  chain . map (toRecursiveTransformation . fromSubstitution) . concat $
    [ complexNumRules,
      zeroOneRules,
      scaleRules,
      dotProductRules,
      distributiveRules,
      piecewiseRules,
      exponentRules,
      rotateRules,
      fourierTransformRules,
      otherRules
    ]

-- | Rules with zero and one
--
--   This includes basic rules such as identity and zero of multiplication and identity of addition.
zeroOneRules :: [Substitution]
zeroOneRules =
  [ one *. x |.~~~~~~> x,
    one * x |.~~~~~~> x,
    x * one |.~~~~~~> x,
    x ^ powerZero |.~~~~~~> one,
    x ^ powerOne |.~~~~~~> x,
    zero * x |.~~~~~~> zero,
    x * zero |.~~~~~~> zero,
    zero *. x |. isReal x ~~~~~~> zero,
    zero *. x |. isComplex x ~~~~~~> zero +: zero,
    x *. zero |.~~~~~~> zero,
    one *. x |.~~~~~~> x,
    x + zero |.~~~~~~> x,
    zero + x |.~~~~~~> x,
    x <.> zero |.~~~~~~> zero,
    zero <.> x |.~~~~~~> zero,
    -- Covector
    zero |*| dx |.~~~~~~> dZero,
    x |*| dZero |.~~~~~~> dZero,
    one |*| dx |.~~~~~~> dx,
    zero |*.| dx |.~~~~~~> dZero,
    x |*.| dZero |.~~~~~~> dZero,
    one |*.| dx |.~~~~~~> dx,
    dZero |.*| x |.~~~~~~> dZero,
    zero |<.>| dx |.~~~~~~> dZero,
    x |<.>| dZero |.~~~~~~> dZero
  ]

-- | Rules related to the scaling operation
--
--   This include rules such as associativity of scaling and moving a negation inside a scaling.
scaleRules :: [Substitution]
scaleRules =
  [ x *. (y *. z) |. sameElementType [x, y] ~~~~~~> (x * y) *. z,
    negate (s *. x) |.~~~~~~> s *. negate x,
    xRe (s *. x) |. isReal s ~~~~~~> s *. xRe x,
    xIm (s *. x) |. isReal s ~~~~~~> s *. xIm x,
    restOfProduct ~* (s *. x) |.~~~~~~> s *. (restOfProduct ~* x),
    -- Covector
    x |*.| (y |*.| dz) |.~~~~~~> (x * y) |*.| dz,
    negate (s |*.| dx) |.~~~~~~> negate s |*.| dx,
    x |*| (s |*.| dy) |.~~~~~~> (s *. x) |*.| dy
  ]

-- | Rules for operations on complex numbers
--
--   This includes complex versions of basic addition and multiplication theorems, as well as
--   simplifying complex numbers with no complex component into real numbers.
complexNumRules :: [Substitution]
complexNumRules =
  [ xRe (x +: y) |.~~~~~~> x,
    xIm (x +: y) |.~~~~~~> y,
    (x +: y) + (u +: v) |.~~~~~~> (x + u) +: (y + v),
    s *. (x +: y) |. isReal s ~~~~~~> (s *. x) +: (s *. y),
    (x +: y) * (z +: w) |.~~~~~~> (x * z - y * w) +: (x * w + y * z),
    negate (x +: y) |.~~~~~~> negate x +: negate y,
    (x +: y) * (zero +: zero) |.~~~~~~> zero +: zero,
    restOfProduct ~* (x +: y) ~* (z +: w) |.~~~~~~> restOfProduct ~* ((x * z - y * w) +: (x * w + y * z)),
    restOfSum ~+ (x +: y) ~+ (u +: v) |.~~~~~~> restOfSum ~+ ((x + u) +: (y + v)),
    (x +: y) *. (z +: w) |.~~~~~~> (x *. z - y *. w) +: (x *. w + y *. z),
    (x +: y) <.> (z +: w) |.~~~~~~> (x <.> z + y <.> w) +: (y <.> z - x <.> w)
  ]

-- | Rules for dot product and scale
--
--   These include mutual associativity of these operations as well as rules for shuffling the order of the two
--   operations to put the real value in front of the expression.
dotProductRules :: [Substitution]
dotProductRules =
  [ (s *. x) <.> y |.~~~~~~> s *. (x <.> y), --
    x <.> (s *. y) |. isReal s ~~~~~~> s *. (x <.> y),
    x <.> ((z +: t) *. y) |.~~~~~~> (z +: negate t) *. (x <.> y), -- Conjugate if the scalar is complex
    x <.> y |. (isScalar x &&. isScalar y) &&. (isReal x &&. isReal y) ~~~~~~> (x * y),
    -- Covector
    (s *. x) |<.>| dy |.~~~~~~> s |*.| (x |<.>| dy),
    x |<.>| (s |*.| dy) |.~~~~~~> s |*.| (x |<.>| dy)
  ]

-- | Rules for distributivity of scale, multiplication and dot product over sumP
--
--   This mostly consists of rules to bring leading multiplicands inside a sum, as one would expect.
distributiveRules :: [Substitution]
distributiveRules =
  [ -- Multiplication
    x * sumP ys |.~~~~~~> sumP (mapL (x *) ys),
    sumP ys * x |.~~~~~~> sumP (mapL (* x) ys),
    restOfProduct ~* sumP ys |.~~~~~~> sumP (mapL (restOfProduct ~*) ys),
    -- Dot product
    x <.> sumP ys |.~~~~~~> sumP (mapL (x <.>) ys),
    sumP ys <.> x |.~~~~~~> sumP (mapL (<.> x) ys),
    -- Scaling
    x *. sumP ys |.~~~~~~> sumP (mapL (x *.) ys),
    sumP ys *. x |.~~~~~~> sumP (mapL (*. x) ys),
    negate (sumP ys) |.~~~~~~> sumP (mapL negate ys),
    -- Covector
    sumP ys |*| dx |.~~~~~~> sumP (mapL (|*| dx) ys),
    x |*| sumP dys |.~~~~~~> sumP (mapL (x |*|) dys),
    --
    sumP ys |<.>| dx |.~~~~~~> sumP (mapL (|<.>| dx) ys),
    x |<.>| sumP dys |.~~~~~~> sumP (mapL (x |<.>|) dys),
    --
    sumP ys |*.| dx |.~~~~~~> sumP (mapL (|*.| dx) ys),
    x |*.| sumP dys |.~~~~~~> sumP (mapL (x |*.|) dys),
    --
    dx |.*| sumP ys |.~~~~~~> sumP (mapL (dx |.*|) ys)
  ]

-- | Rules for Fourier transform
--
--   This includes common Fourier transform theorems, including linearity, as well as simplfications that will reduce
--   computational complexity (for example, setting the imaginary-valued FT of a real-valued FT of x to 0).
-- TODO: TwiceImFT and TwiceReFT should be in Optimize module ?
fourierTransformRules :: [Substitution]
fourierTransformRules =
  [ reFT (x +: y) |.~~~~~~> reFT x - imFT y,
    imFT (x +: y) |.~~~~~~> imFT x + reFT y,
    reFT (sumP xs) |.~~~~~~> sumP (mapL reFT xs),
    imFT (sumP xs) |.~~~~~~> sumP (mapL imFT xs),
    -- Real
    reFT zero |.~~~~~~> zero,
    imFT zero |.~~~~~~> zero,
    twiceImFT zero |.~~~~~~> zero,
    twiceReFT zero |.~~~~~~> zero,
    imFT (reFT x) |. isReal x ~~~~~~> zero,
    reFT (imFT x) |. isReal x ~~~~~~> zero,
    reFT (s *. x) |. isReal s ~~~~~~> s *. reFT x,
    imFT (s *. x) |. isReal s ~~~~~~> s *. imFT x,
    reFT (reFT x) |. isReal x ~~~~~~> twiceReFT x,
    imFT (imFT x) |. isReal x ~~~~~~> twiceImFT x,
    -- Covector
    reFT dZero |.~~~~~~> dZero,
    imFT dZero |.~~~~~~> dZero,
    twiceImFT dZero |.~~~~~~> dZero,
    twiceReFT dZero |.~~~~~~> dZero,
    imFT (reFT dx) |. isCovector dx ~~~~~~> dZero,
    reFT (imFT dx) |. isCovector dx ~~~~~~> dZero,
    reFT (s |*.| dx) |.~~~~~~> s |*.| reFT x,
    imFT (s |*.| dx) |.~~~~~~> s |*.| imFT x,
    reFT (dx |.*| y) |.~~~~~~> dx |.*| reFT y,
    imFT (dx |.*| y) |.~~~~~~> dx |.*| imFT y
  ]

-- | Rules for piecewise functions
--
--   Currently consists of a single rule to select the first function in the case where the multiple pieces of the
--   function have the same domain.
piecewiseRules :: [Substitution]
piecewiseRules =
  [ piecewise_ condition branches |. allTheSame branches ~~~~~~> headL branches
  ]

-- | Rules for exponentiation and log
--
--   This includes simplifying exponents applied to logs (and vice versa), as well as setting the exponent of zero equal
--   to one.
exponentRules :: [Substitution]
exponentRules =
  [ exp (log x) |.~~~~~~> x, --
    log (exp x) |.~~~~~~> x, --
    exp zero |.~~~~~~> one
  ]

-- | Miscellaneous rules
otherRules :: [Substitution]
otherRules =
  [ negate x |. isReal x ||. isComplex x ~~~~~~> (-1 :: Pattern) *. x,
    negate dx |. isCovector dx ~~~~~~> (-1 :: Pattern) |*.| x,
    (x ^ alpha) ^ beta |.~~~~~~> x ^ (alpha * beta)
  ]

-- | Rules related to rotations
--
--   This includes the linearity of rotations and combining two composed rotations together into one rotation.
rotateRules :: [Substitution]
rotateRules =
  [ rotate amount (s *. x) |.~~~~~~> s *. rotate amount x,
    rotate amount1 (rotate amount2 x) |.~~~~~~> rotate (amount1 + amount2) x,
    rotate amount x |. zeroAmount amount ~~~~~~> x,
    rotate amount (sumP xs) |.~~~~~~> sumP (mapL (rotate amount) xs),
    rotate amount (productP xs) |.~~~~~~> productP (mapL (rotate amount) xs),
    rotate amount (x +: y) |.~~~~~~> rotate amount x +: rotate amount y,
    rotate amount1 x <.> rotate amount2 y |. sameAmount amount1 amount2 ~~~~~~> (x <.> y),
    rotate amount zero |.~~~~~~> zero,
    -- Covector
    rotate amount (s |*.| dx) |.~~~~~~> s |*.| rotate amount dx,
    rotate amount1 x |<.>| rotate amount2 y |. sameAmount amount1 amount2 ~~~~~~> (x |<.>| y),
    rotate amount dZero |.~~~~~~> dZero
  ]

-- | Identity and zero laws for 'Sum' and 'Mul'.
--   This includes removing the unnecessary ones from a multiplication or zeroes from an addition.
zeroOneSumProdRules :: Modification
zeroOneSumProdRules exp@(mp, n) =
  case retrieveOp n mp of
    Sum ns
      -- to make sure filter (not . isZero mp) ns is not empty
      | all (isZero mp) ns -> just $ head ns
      -- if the sumP has any zero, remove them
      -- sum(x, y, z, 0, t, 0) = sum(x, y, z, t)
      | any (isZero mp) ns ->
        sum_ . map just . filter (not . isZero mp) $ ns
      | any (isDZero mp) ns ->
        sum_ . map just . filter (not . isDZero mp) $ ns
    Mul ns
      -- to make sure filter (not . isOne mp) ns is not empty
      | all (isOne mp) ns -> just $ head ns
      -- if the product has any one, remove them
      -- product(x, y, z, 1, t, 1) = product(x, y, z, t)
      | any (isOne mp) ns ->
        product_ . map just . filter (not . isOne mp) $ ns
      -- if any is zero, collapse to zero
      -- product(x, y, z, 0, t, u, v) = 0
      | nId : _ <- filter (isZero mp) ns -> just nId
    -- if the prod contains any prod, just flatten them out
    -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
    _ -> just n

-- | Rules for collapsing 'Sum' and 'Mul'
--
--   For example, if a sumP or product consists of only one entry, it can be extracted.
collapseSumProdRules :: Modification
collapseSumProdRules exp@(mp, n) =
  case retrieveOp n mp of
    Sum [nID] -> just nID
    Mul [nID] -> just nID
    _ -> just n

-- | Rules for flattening 'Sum' and 'Mul'.
--
--   For example, if sumP or product contains sub-sum or sub-product, flatten them out. This is analogous to the `concat`
--   function on lists.
flattenSumProdRules :: Modification
flattenSumProdRules exp@(mp, n) =
  case retrieveOp n mp of
    Sum ns ->
      -- if the sumP contains any sumP, just flatten them out
      -- sum(x, sum(y, z), sum(t, u, v)) = sum(x, y, z, t, u, v)
      sum_ . map just . concatMap (pullSumOperands mp) $ ns
    Mul ns ->
      -- if the prod contains any prod, just flatten them out
      -- product(x, product(y, z), product(t, u, v)) = product(x, y, z, t, u, v)
      product_ . map just . concatMap (pullProdOperands mp) $ ns
    _ -> just n

-- | Rules for grouping constants
--   If there is more than one constant in a sumP or a product, group them together to reduce complexity.
groupConstantsRules :: Modification
groupConstantsRules exp@(mp, n) =
  let shape = retrieveShape n mp
   in case retrieveOp n mp of
        Sum ns
          | Just (_, cs) <- pullConstants mp ns,
            length cs > 1,
            let total = const_ shape . Prelude.sum $ cs ->
            sum_ $ total : (map just . filter (not . isConstant mp) $ ns)
        Mul ns
          | Just (_, cs) <- pullConstants mp ns,
            length cs > 1,
            let total = const_ shape . Prelude.product $ cs ->
            product_ $ total : (map just . filter (not . isConstant mp) $ ns)
        _ -> just n

-- | Rules for combining and reducing the number of terms in 'Sum'
--
--   This includes the following scenarios:
--   * Sum [x,(-1) *. x,y] -> Sum [y]
--   * Sum [2 *. x, (-1) *. x,y] -> Sum [x,y]
--   * Sum [x,x,y] -> Sum [2 *. x,y]
combineTermsRules :: Modification
combineTermsRules exp@(mp, n)
  | Sum ns <- retrieveOp n mp =
    sum_ . map (toDiff . combine) . groupBy fn . sortWith fst . map cntAppr $ ns
  | otherwise = just n
  where
    cntAppr nId
      | Scale scalerN scaleeN <- retrieveOp nId mp,
        Const val <- retrieveOp scalerN mp =
        (scaleeN, val)
      | Neg negateeN <- retrieveOp nId mp = (negateeN, -1)
      | otherwise = (nId, 1)
    combine xs = (fst $ head xs, Prelude.sum $ map snd xs)
    fn x y = fst x == fst y
    toDiff :: (Int, Double) -> State ExpressionMap NodeID
    toDiff (nId, val)
      | val == 1 = just nId
      | retrieveElementType nId mp == Covector = num_ val |*.| just nId
      | otherwise = num_ val *. just nId

-- | Rules for combining and reducing the numbers of terms in 'Mul'
--
--   This includes the following scenarios:
--   * Mul(x^(-1) * x,y) -> y
--   * Mul(x,x,y) -> Mul(x^2,y), but we don't group Sum or Complex
combineTermsRulesProd :: Modification
combineTermsRulesProd exp@(mp, n)
  | Mul ns <- retrieveOp n mp =
    product_ . map (toDiff . combine) . groupBy fn . sortWith fst . map cntAppr $ ns
  | otherwise = just n
  where
    cntAppr nId
      | Power value powerel <- retrieveOp nId mp = (powerel, value)
      | otherwise = (nId, 1)
    combine xs = (fst $ head xs, Prelude.sum $ map snd xs)
    fn (x, px) (y, py)
      | Sum _ <- retrieveOp x mp = False
      | Sum _ <- retrieveOp y mp = False
      | C <- retrieveElementType x mp = False
      | C <- retrieveElementType y mp = False
      | otherwise = x == y
    toDiff (nId, val)
      | val == 1 = just nId
      | otherwise = just nId ^ val

-- | Rules for combining powers of power
--
--   This includes rules like:
--   * (x^2)^3 -> x^6
--   * (x^2)^-1 -> x^-2
combinePowerRules :: Modification
combinePowerRules exp@(mp, n)
  | Power outerVal outerN <- retrieveOp n mp,
    Power innerVal innerN <- retrieveOp outerN mp =
    just innerN ^ (outerVal * innerVal)
  | otherwise = just n

-- | Rules for power of sumP and power of 'RealImag'
--
--   * (a+b)^2 should be (a+b)*(a+b)
--   * (a +: b) ^ 2 should be (a +: b) * (a +: b)
powerSumRealImagRules :: Modification
powerSumRealImagRules exp@(mp, n)
  | Power val nId <- retrieveOp n mp,
    isSumOrRealImag nId =
    replicateMul val nId
  | otherwise = just n
  where
    isSumOrRealImag nId
      | Sum _ <- retrieveOp nId mp = True
      | RealImag _ _ <- retrieveOp nId mp = True
      | otherwise = False
    replicateMul val nId
      | val > 1 = product_ . replicate val $ just nId
      | val < -1 = (^ (-1)) . product_ . replicate (- val) . just $ nId
      | otherwise = just nId

-- | Rules for power product
--
--   * (a*b*c)^k should be a^k * b^k * c^k
powerProdRules :: Modification
powerProdRules exp@(mp, n)
  | Power val nId <- retrieveOp n mp,
    Mul args <- retrieveOp nId mp =
    product_ . map ((^ val) . just) $ args
  | otherwise = just n

-- | Rules for power scale
--
--   * (a*.b)^2 should be a^2 *. b^2
powerScaleRules :: Modification
powerScaleRules exp@(mp, n)
  | Power val nId <- retrieveOp n mp,
    Scale scalar scalee <- retrieveOp nId mp,
    val > 0 =
    (just scalar ^ val) *. (just scalee ^ val)
  | otherwise = just n

-- | Rules for combining scalar scales and multiplications
--
--   * (a *. x) * (b *. y) * (c *. z) --> (a * b * c) *. (x * y * z) (if all are real)
combineRealScalarRules :: Modification
combineRealScalarRules exp@(mp, n)
  | Mul ns <- retrieveOp n mp,
    retrieveElementType n mp == R,
    let extracted = map extract ns,
    any (isJust . snd) extracted =
    let combinedScalars = product_ (mapMaybe snd extracted)
        combinedScalees = product_ $ map fst extracted
     in combinedScalars *. combinedScalees
  | otherwise = just n
  where
    extract nId
      | Scale scalar scalee <- retrieveOp nId mp =
        (just scalee, Just $ just scalar)
      | Neg negatee <- retrieveOp nId mp,
        retrieveElementType nId mp == R =
        (just negatee, Just $ num_ (-1))
      | otherwise = (just nId, Nothing)

-- | Rules that are applied in specific scenarios if possible
--
--   For instance, we can combine constants in 'Sum' and 'Mul' using regular Haskell arithmetic.
evaluateIfPossibleRules :: Modification
evaluateIfPossibleRules exp@(mp, n) =
  case (retrieveElementType n mp, node, pulledVals) of
    (R, Sum _, Just vals) -> res $ Prelude.sum vals
    (R, Mul _, Just vals) -> res $ Prelude.product vals
    (R, Scale _ _, Just [val1, val2]) -> res $ val1 * val2
    (R, Neg _, Just [val])
      | val /= 0 -> res (- val)
      | otherwise -> res 0
    (R, Power x _, Just [val]) -> res $ val ** fromIntegral x
    (R, InnerProd arg1 arg2, Just [val1, val2]) ->
      res $ val1 * val2 * (fromIntegral . Prelude.product $ retrieveShape arg1 mp)
    (R, Rotate _ _, Just [val]) -> res val
    -- TODO: sin, sos, ...
    _ -> just n
  where
    (shape, _, node) = retrieveNode n mp
    getVal nId
      | Const val <- retrieveOp nId mp = Just val
      | otherwise = Nothing
    pulledVals = mapM getVal . opArgs $ node
    res = const_ shape

-- | Rules to normalize rotations
--
--   This ensures that rotateAmount in each direction always lie within (0, dim - 1)
normalizeRotateRules :: Modification
normalizeRotateRules exp@(mp, n)
  | (shape, _, Rotate amount arg) <- retrieveNode n mp = rotate (zipWith mod amount shape) $ just arg
  | otherwise = just n

-- | Rules for negative zeroes
--
--   This is to avoid having `Const (-0.0)` show up, which is considered different than `Const (0.0)`.
--   Thus, instead we convert those to `Const (0.0)`.
negativeZeroRules :: Modification
negativeZeroRules exp@(mp, n)
  | (shape, _, Const val) <- retrieveNode n mp,
    val == 0.0 || val == (-0.0) =
    const_ shape 0
  | otherwise = just n

-- | Rules for piecewise functions of 'RealImag'
--
--   In particular, Piecewise of 'RealImag' is the same as 'RealImag' of piecewise functions.
--
--   * if a > 2 then x +: y else m +: n --> (if a > 2 then x else m) +: (if a > 2 then y else n)
expandPiecewiseRealImag :: Modification
expandPiecewiseRealImag exp@(mp, n)
  | Piecewise marks condition branches <- retrieveOp n mp,
    Just reIms <- mapM extract branches =
    let (reIds, imIds) = unzip reIms
        cdt = just condition
        res = map just reIds
        ims = map just imIds
     in piecewise marks cdt res +: piecewise marks cdt ims
  | otherwise = just n
  where
    extract nId
      | RealImag re im <- retrieveOp nId mp = Just (re, im)
      | otherwise = Nothing

-- | Rules for expanding piecewise functions
--
--   For example: `if x < 1 then x + 1 else x + y = (x + 1) * (if x < 1 then 1 else 0) + (x + y) * (if x < 1 then 0 else 1)`
pullOutPiecewiseRules :: Modification
pullOutPiecewiseRules exp@(mp, n) =
  case retrieveOp n mp of
    Piecewise marks condition branches
      | et == R || et == Covector,
        not (isAllZeroExceptOne branches) ->
        let branchWithPiecewise idx branch =
              let newPiecewiseBranches =
                    -- [1, 0, 0 .. ]
                    replicate idx zero ++ [one] ++ replicate (length branches - idx - 1) zero
                  allZerosOneOne =
                    -- piecewise marks condition [1, 0, 0, ..]
                    piecewise marks (just condition) newPiecewiseBranches
               in case et of
                    R -> allZerosOneOne * just branch
                    Covector -> allZerosOneOne |*| just branch
         in sum_ . zipWith branchWithPiecewise [0 ..] $ branches
    _ -> just n
  where
    isAllZeroExceptOne branches
      | [shouldBeOne] <- filter (not . isZero mp) branches,
        isOne mp shouldBeOne =
        True
      | otherwise = False
    et = retrieveElementType n mp
    shape = retrieveShape n mp
    one = const_ shape 1
    zero = const_ shape 0

-- | Rules for advanced FT simplification
--
-- twiceReFT is the same as (xRe . ft) . (xRe . ft). The former has better performance than the latter for the same
-- operation. We can simplify this further as `twiceReFT(x) + twiceImFT(x) = (size(x) / 2) *. x`.
twiceReFTAndImFTRules :: Modification
twiceReFTAndImFTRules exp@(mp, n)
  | Sum sumands <- retrieveOp n mp,
    retrieveElementType n mp == R,
    Just (scaleFactor, twiceReFTid, innerArg) <- firstJust isTwiceReFT sumands,
    Just twiceImFTid <- find (isTwiceImFTof innerArg scaleFactor) sumands =
    let rest = map just . filter (\x -> x /= twiceReFTid && x /= twiceImFTid) $ sumands
        totalScaleFactor = scaleFactor * fromIntegral (Prelude.product $ retrieveShape innerArg mp)
        scalar = num_ totalScaleFactor
        scaled = scalar *. just innerArg
     in sum_ $ scaled : rest
  | otherwise = just n
  where
    isTwiceReFT nId
      | TwiceReFT inner <- retrieveOp nId mp = Just (1, nId, inner)
      | Scale scalarId scaleeId <- retrieveOp nId mp,
        retrieveElementType nId mp == R,
        Const val <- retrieveOp scalarId mp,
        TwiceReFT inner <- retrieveOp scaleeId mp =
        Just (val, nId, inner)
      | otherwise = Nothing
    isTwiceImFTof innerArg scaleFactor nId
      | TwiceImFT x <- retrieveOp nId mp,
        scaleFactor == 1 && x == innerArg =
        True
      | Scale scalarId scaleeId <- retrieveOp nId mp,
        retrieveElementType nId mp == R,
        Const val <- retrieveOp scalarId mp,
        val == scaleFactor,
        TwiceImFT x <- retrieveOp scaleeId mp,
        x == innerArg =
        True
      | otherwise = False

-- | Re-order operands in associative-commutative operators like Sum or Mul
-- or commutative binary like InnerProd for real
reorderOperands :: Modification
reorderOperands exp@(mp, n)
  | Sum operands <- retrieveOp n mp,
    let sortedOperands = sortOperands operands,
    sortedOperands /= operands =
    sum_ (map just sortedOperands)
  | Mul operands <- retrieveOp n mp,
    let sortedOperands = sortOperands operands,
    sortedOperands /= operands =
    product_ (map just sortedOperands)
  | InnerProd o1 o2 <- retrieveOp n mp,
    retrieveElementType n mp == R,
    let [s1, s2] = sortOperands [o1, o2],
    [s1, s2] /= [o1, o2] =
    just s1 <.> just s2
  | otherwise = just n
  where
    weight nID = nodeTypeWeight $ retrieveOp nID mp
    opType nID1 nID2 = weight nID1 == weight nID2
    sortOperands os = concatMap (sortWith id) . groupBy opType . sortWith weight $ os
