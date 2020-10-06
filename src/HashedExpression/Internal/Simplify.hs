-- |
-- Module      :  HashedExpression.Internal.SImplify
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Simplifying expressions
module HashedExpression.Internal.Simplify (simplify, simplifyUnwrapped) where

import Control.Monad.State.Strict
import Data.List (partition, sort)
import Data.List.Extra (groupOn)
import GHC.Exts (sortWith)
import HashedExpression.Internal
import HashedExpression.Internal.Base
import HashedExpression.Internal.MonadExpression
import HashedExpression.Internal.Node
import HashedExpression.Internal.Pattern
import HashedExpression.Internal.Rewrite
import HashedExpression.Utils
import Prelude hiding ((**), (^))
import qualified Prelude

simplifyUnwrapped :: Expr -> Expr
simplifyUnwrapped = removeUnreachable . apply
  where
    apply =
      multipleTimes 1000 . toRecursiveTransformation . chainModifications $
        [ zeroOneSumProdRules,
          collapseSumProdRules,
          flattenSumProdRules,
          groupConstantsRules,
          combineTermsRules,
          combineTermsRulesProd,
          evaluateIfPossibleRules,
          normalizeRotateRules,
          negativeZeroRules,
          reorderOperands
        ]
          ++ map
            fromSubstitution
            ( concat
                [ toMultiplyIfPossible,
                  zeroOneRules,
                  scaleRules,
                  complexNumRules,
                  dotProductRules,
                  piecewiseRules,
                  exponentRules,
                  otherRules,
                  rotateRules
                ]
            )

simplify :: IsExpression e => e -> e
simplify = wrapExpression . simplifyUnwrapped . asExpression

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

toMultiplyIfPossible :: [Substitution]
toMultiplyIfPossible =
  [ x *. y |. isReal y &&. isScalar y &&. isNotConst x ~~> x * y,
    x <.> y |. isReal x &&. isScalar x &&. isScalar y ~~> x * y
  ]

-- | Rules with zero and one
--
--   This includes basic rules such as identity and zero of multiplication and identity of addition.
zeroOneRules :: [Substitution]
zeroOneRules =
  [ one *. x |.~~> x,
    one * x |.~~> x,
    x * one |.~~> x,
    x ^ 0 |.~~> one,
    x ^ 1 |.~~> x,
    zero * x |.~~> zero,
    x * zero |.~~> zero,
    zero *. x |. isReal x ~~> zero,
    zero *. x |. isComplex x ~~> zero +: zero,
    x *. zero |.~~> zero,
    one *. x |.~~> x,
    x + zero |.~~> x,
    zero + x |.~~> x,
    x <.> zero |.~~> zero,
    zero <.> x |.~~> zero
  ]

-- | Rules related to the scaling operation
--
--   This include rules such as associativity of scaling and moving a negation inside a scaling.
scaleRules :: [Substitution]
scaleRules =
  [ x *. (y *. z) |. sameElementType [x, y] ~~> (x * y) *. z,
    xRe (s *. x) |. isReal s ~~> s *. xRe x,
    xIm (s *. x) |. isReal s ~~> s *. xIm x,
    restOfProduct ~* (s *. x) |.~~> s *. (restOfProduct ~* x)
  ]

-- | Rules for operations on complex numbers
--
--   This includes complex versions of basic addition and multiplication theorems, as well as
--   simplifying complex numbers with no complex component into real numbers.
complexNumRules :: [Substitution]
complexNumRules =
  [ xRe (x +: y) |.~~> x,
    xIm (x +: y) |.~~> y,
    (x +: y) * (zero +: zero) |.~~> zero +: zero
  ]

-- | Rules for dot product and scale
--
--   These include mutual associativity of these operations as well as rules for shuffling the order of the two
--   operations to put the real value in front of the expression.
dotProductRules :: [Substitution]
dotProductRules =
  [ (s *. x) <.> y |.~~> s *. (x <.> y), --
    x <.> (s *. y) |. isReal s ~~> s *. (x <.> y),
    x <.> ((z +: t) *. y) |.~~> (z +: negate t) *. (x <.> y) -- Conjugate if the scalar is complex
  ]

-- | Rules for piecewise functions
--
--   Currently consists of a single rule to select the first function in the case where the multiple pieces of the
--   function have the same domain.
piecewiseRules :: [Substitution]
piecewiseRules =
  [ piecewise_ condition branches |. allTheSame branches ~~> headL branches
  ]

-- | Rules for exponentiation and log
--
--   This includes simplifying exponents applied to logs (and vice versa), as well as setting the exponent of zero equal
--   to one.
exponentRules :: [Substitution]
exponentRules =
  [ exp (log x) |.~~> x, --
    log (exp x) |.~~> x, --
    exp zero |.~~> one
  ]

-- | Miscellaneous rules
otherRules :: [Substitution]
otherRules =
  [ negate x |.~~> (-1 :: Pattern) *. x,
    (x ^ alpha) ^ beta |.~~> x ^ (alpha * beta)
  ]

-- | Rules related to rotations
--
--   This includes the linearity of rotations and combining two composed rotations together into one rotation.
rotateRules :: [Substitution]
rotateRules =
  [ rotate amount (s *. x) |.~~> s *. rotate amount x,
    rotate amount1 (rotate amount2 x) |.~~> rotate (amount1 + amount2) x,
    rotate amount x |. zeroAmount amount ~~> x,
    rotate amount1 x <.> rotate amount2 y |. sameAmount amount1 amount2 ~~> (x <.> y),
    rotate amount zero |.~~> zero
  ]

-- | Identity and zero laws for 'Sum' and 'Mul'.
--   This includes removing the unnecessary ones from a multiplication or zeroes from an addition.
zeroOneSumProdRules :: Modification
zeroOneSumProdRules exp@(mp, n) =
  case retrieveOp n mp of
    Sum ns
      -- if the sumP has any zero, remove them
      -- sum(x, y, z, 0, t, 0) = sum(x, y, z, t)
      | (x : _, []) <- partition (isZero mp) ns -> just x
      | (_, nonZeros) <- partition (isZero mp) ns -> sum_ . map just $ nonZeros
    Mul ns
      -- if the product has any one, remove them
      -- product(x, y, z, 1, t, 1) = product(x, y, z, t)
      | (x : _, []) <- partition (isOne mp) ns -> just x
      | (_, nonOnes) <- partition (isOne mp) ns -> product_ . map just $ nonOnes
      -- if any is zero, collapse to zero
      -- product(x, y, z, 0, t, u, v) = 0
      | nId : _ <- filter (isZero mp) ns -> just nId
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
            let total = const_ shape . sum $ cs ->
            sum_ $ total : (map just . filter (not . isConstant mp) $ ns)
        Mul ns
          | Just (_, cs) <- pullConstants mp ns,
            rest@(x : _) <- filter (not . isConstant mp) ns,
            let scalar = num_ . product $ cs ->
            scalar *. product_ (map just rest)
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
    sum_ . map (build . combine) . groupOn fst . sortWith fst . map count $ ns
  | otherwise = just n
  where
    count nId
      | Scale scalerN scaleeN <- retrieveOp nId mp,
        Const val <- retrieveOp scalerN mp =
        (scaleeN, val)
      | Neg negateeN <- retrieveOp nId mp = (negateeN, -1)
      | otherwise = (nId, 1)
    combine xs = (fst $ head xs, sum $ map snd xs)
    build :: (NodeID, Double) -> Rewrite NodeID
    build (nId, val)
      | val == 1 = just nId
      | otherwise = num_ val *. just nId

-- | Rules for combining and reducing the numbers of terms in 'Mul'
--   This includes the following scenarios:
--   * Mul(x^(-1) * x,y) -> y
--   * Mul(x,x,y) -> Mul(x^2,y)
combineTermsRulesProd :: Modification
combineTermsRulesProd exp@(mp, n)
  | Mul ns <- retrieveOp n mp =
    product_ . map (build . combine) . groupOn fst . sortWith fst . map count $ ns
  | otherwise = just n
  where
    count nId
      | Power value arg <- retrieveOp nId mp = (arg, value)
      | otherwise = (nId, 1)
    combine xs = (fst $ head xs, sum $ map snd xs)
    build (nId, val)
      | val == 1 = just nId
      | otherwise = just nId ^ val

-- | Rules that are applied in specific scenarios if possible
--
--   For instance, we can combine constants in 'Sum' and 'Mul' using regular Haskell arithmetic.
evaluateIfPossibleRules :: Modification
evaluateIfPossibleRules exp@(mp, n) =
  case (retrieveElementType n mp, node, pulledVals) of
    (R, Sum _, Just vals) -> res $ sum vals
    (R, Mul _, Just vals) -> res $ product vals
    (R, Scale _ _, Just [val1, val2]) -> res $ val1 * val2
    (R, Neg _, Just [val])
      | val /= 0 -> res (- val)
      | otherwise -> res 0
    (R, Power x _, Just [val]) -> res $ val Prelude.** fromIntegral x
    (R, InnerProd arg1 arg2, Just [val1, val2]) ->
      res $ val1 * val2 * (fromIntegral . product $ retrieveShape arg1 mp)
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
    weight nID = opTypeWeight $ retrieveOp nID mp
    sortOperands os = concatMap sort . groupOn weight . sortWith weight $ os
