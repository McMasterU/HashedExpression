module HashedExpression.Internal.Rewrite where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.ST.Strict
import Control.Monad.State.Strict
import Data.Array.MArray
import Data.Array.ST
import qualified Data.Array.Unboxed as UA
import Data.Data (Typeable)
import Data.Graph (buildG, topSort)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (find, foldl', groupBy, sort, sortBy, sortOn)
import Data.List.Extra (firstJust)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.STRef.Strict
import Data.Set (Set, empty, insert, member)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import HashedExpression.Internal hiding (const_, just, num_, product_, sum_)
import HashedExpression.Internal.Context
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Utils
import Prelude hiding ((^))

type Modification1 = (ExpressionMap, NodeID) -> State ExpressionMap NodeID

toTransformation :: Modification1 -> Transformation
toTransformation modify exp =
  let (nID, newMp) = runState (modify exp) (fst exp)
   in (newMp, nID)

toRecursiveTransformation ::
  Modification1 ->
  -- | resulting rule applied to every 'Node'
  Transformation
toRecursiveTransformation smp exp@(mp, headN) = (finalMap, fromJust $ IM.lookup headN finalSub)
  where
    -------------------------------------------------------------------------------
    topoOrder :: [NodeID]
    topoOrder = topologicalSort exp
    -------------------------------------------------------------------------------
    f :: IM.IntMap NodeID -> NodeID -> State ExpressionMap (IM.IntMap NodeID)
    f sub nID = do
      curMp <- get
      let oldNode = retrieveNode nID curMp
          newNode = mapNode (toTotal sub) oldNode
      cID <- if newNode == oldNode then pure nID else introduceNode newNode
      updatedMp <- get
      appliedRuleNodeID <- smp (updatedMp, cID)
      return $ IM.insert nID appliedRuleNodeID sub
    (finalSub, finalMap) = runState (foldM f IM.empty topoOrder) mp

instance MonadExpression (State ExpressionMap) where
  introduceNode node = do
    mp <- get
    let nID = hashNode (checkHashFromMap mp) node
    modify' $ IM.insert nID node
    return nID

  getContextMap = get

just :: NodeID -> State ExpressionMap NodeID
just = return

sum_ :: [State ExpressionMap NodeID] -> State ExpressionMap NodeID
sum_ ops = sequence ops >>= perform (Nary specSum)

product_ :: [State ExpressionMap NodeID] -> State ExpressionMap NodeID
product_ ops = sequence ops >>= perform (Nary specMul)

const_ :: Shape -> Double -> State ExpressionMap NodeID
const_ shape val = introduceNode (shape, R, Const val)

num_ :: Double -> State ExpressionMap NodeID
num_ = const_ []

-- | Identity and zero laws for 'Sum' and 'Mul'.
--   This includes removing the unnecessary ones from a multiplication or zeroes from an addition.
zeroOneSumProdRules :: Modification1
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
collapseSumProdRules :: Modification1
collapseSumProdRules exp@(mp, n) =
  case retrieveOp n mp of
    Sum [nID] -> just nID
    Mul [nID] -> just nID
    _ -> just n

-- | Rules for flattening 'Sum' and 'Mul'.
--
--   For example, if sumP or product contains sub-sum or sub-product, flatten them out. This is analogous to the `concat`
--   function on lists.
flattenSumProdRules :: Modification1
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
groupConstantsRules :: Modification1
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
combineTermsRules :: Modification1
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
combineTermsRulesProd :: Modification1
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
combinePowerRules :: Modification1
combinePowerRules exp@(mp, n)
  | Power outerVal outerN <- retrieveOp n mp,
    Power innerVal innerN <- retrieveOp outerN mp =
    just innerN ^ (outerVal * innerVal)
  | otherwise = just n

-- | Rules for power of sumP and power of 'RealImag'
--
--   * (a+b)^2 should be (a+b)*(a+b)
--   * (a +: b) ^ 2 should be (a +: b) * (a +: b)
powerSumRealImagRules :: Modification1
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
powerProdRules :: Modification1
powerProdRules exp@(mp, n)
  | Power val nId <- retrieveOp n mp,
    Mul args <- retrieveOp nId mp =
    product_ . map ((^ val) . just) $ args
  | otherwise = just n

-- | Rules for power scale
--
--   * (a*.b)^2 should be a^2 *. b^2
powerScaleRules :: Modification1
powerScaleRules exp@(mp, n)
  | Power val nId <- retrieveOp n mp,
    Scale scalar scalee <- retrieveOp nId mp,
    val > 0 =
    (just scalar ^ val) *. (just scalee ^ val)
  | otherwise = just n

-- | Rules for combining scalar scales and multiplications
--
--   * (a *. x) * (b *. y) * (c *. z) --> (a * b * c) *. (x * y * z) (if all are real)
combineRealScalarRules :: Modification1
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
evaluateIfPossibleRules :: Modification1
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
normalizeRotateRules :: Modification1
normalizeRotateRules exp@(mp, n)
  | (shape, _, Rotate amount arg) <- retrieveNode n mp = rotate (zipWith mod amount shape) $ just arg
  | otherwise = just n

-- | Rules for negative zeroes
--
--   This is to avoid having `Const (-0.0)` show up, which is considered different than `Const (0.0)`.
--   Thus, instead we convert those to `Const (0.0)`.
negativeZeroRules :: Modification1
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
expandPiecewiseRealImag :: Modification1
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
pullOutPiecewiseRules :: Modification1
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
twiceReFTAndImFTRules :: Modification1
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
reorderOperands :: Modification1
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
