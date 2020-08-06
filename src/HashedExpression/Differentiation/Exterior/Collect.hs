-- |
-- Module      :  HashedExpression.Differentiation.Exterior.Collect
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module exists solely to factor terms around their differentials. When properly factored, the term multiplying
-- a differential (say dx) is it's corresponding parital derivative (i.e derivative w.r.t x)
module HashedExpression.Differentiation.Exterior.Collect
  ( collectDifferentials,
  )
where

import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
  ( elemIndex,
    foldl',
    group,
    groupBy,
    intercalate,
    partition,
    sort,
  )
import Data.List.NonEmpty (groupWith)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceShow, traceShowId)
import GHC.Exts (sortWith)
import HashedExpression.Differentiation.Exterior.Normalize
import HashedExpression.Internal hiding (const_, just, num_, product_, sum_)
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Hash
import HashedExpression.Internal.Node
import HashedExpression.Internal.OperationSpec
import HashedExpression.Internal.Pattern
import HashedExpression.Internal.Rewrite
import HashedExpression.Internal.Utils
import HashedExpression.Operation (constant)
import HashedExpression.Prettify
import Prelude hiding (product, sum, (^))
import qualified Prelude

-- | Predefined holes using for pattern matching with 'Pattern'
[p, q, r, s, t, u, v, w, x, y, z, condition] = map PHole [1 .. 12]

[dx, dy, dz] = map PHole [20, 21, 22]

-- | Predefined holes used for pattern matching with 'PRotateAmountHole'
[amount, amount1, amount2, amount3] = map PRotateAmountHole [1 .. 4]

-- | Predefined holes used for pattern matching with 'PListHole'
[xs, ys] = map (PListHole id) [1, 2]

-- | Factors terms around differentials. When computing derivatives with 'exteriorDerivative', differential operators are
--   dispersed among the expression as computed, to determine the parital derivatives you need to collect like terms.
--   For example:
--
--   @
--    y*dx + x*dy + 2.0*dx
--    => (2.0+y)*dx + x*dy
--   @
--
--   Note: after normalize:
--
--     * No complex in the input (:+, xRe, xIm)
--
--     * Scale is pushed to the outer most layer and real scalars are group together in a product
collectDifferentials :: Expression Scalar Covector -> Expression Scalar Covector
collectDifferentials = wrap . applyRules . unwrap . normalize
  where
    applyRules =
      chain
        [ separateDVarAlone,
          toTransformation groupByDVar,
          aggregateByDVar,
          normalizeEachPartialDerivative,
          removeUnreachable
        ]

inspect :: Transformation
inspect exp = traceShow (debugPrint exp) exp

-- | Move dVar out of operations (like reFT) that would prevent factoring
separateDVarAlone :: Transformation
separateDVarAlone =
  multipleTimes 1000 . toRecursiveTransformation . chainModifications . map fromSubstitution $
    [ x |<.>| (y |*| dz) |.~~~~~~> (x * y) |<.>| dz,
      s |*| (x |<.>| dy) |.~~~~~~> (s *. x) |<.>| dy,
      s |*| (x |*| dy) |.~~~~~~> (s * x) |*| dy,
      x |<.>| rotate amount dy |.~~~~~~> (rotate (negate amount) x |<.>| dy),
      x |<.>| reFT dy |.~~~~~~> reFT x |<.>| dy,
      x |<.>| imFT dy |.~~~~~~> imFT x |<.>| dy,
      x |<.>| twiceReFT dy |.~~~~~~> twiceReFT x |<.>| dy,
      x |<.>| twiceImFT dy |.~~~~~~> twiceImFT x |<.>| dy
    ]

-- | Group a sum to many sums, each sum is corresponding to a DVar, preparing for aggregateByDVar
-- (f * dx + h * dy + dx + t1 <.> dx1 + f1 <.> dx1) -->
--   ((f * dx + 1 * dx) + (h * dy) + (t1 <.> dx1 + f1 <.> dx1)
groupByDVar :: Modification
groupByDVar exp@(mp, n) =
  case retrieveOp n mp of
    Sum ns
      | retrieveElementType n mp == Covector ->
        let groups = groupBy sameDVar . sortWith getDVar . filter (not . isZero mp) $ ns
         in sum_ . map (sum_ . map mulOneIfAlone) $ groups
    _ -> mulOneIfAlone n
  where
    getDVar :: Int -> String
    getDVar nId
      | DVar name <- retrieveOp nId mp = name
      | MulD _ cId <- retrieveOp nId mp,
        retrieveElementType nId mp == Covector,
        DVar name <- retrieveOp cId mp =
        name
      | InnerProdD _ cId <- retrieveOp nId mp,
        retrieveElementType nId mp == Covector,
        DVar name <- retrieveOp cId mp =
        name
      | otherwise = error $ "Collect D: " ++ debugPrint (mp, nId) ++ " " ++ show (retrieveOp nId mp)
    sameDVar :: Int -> Int -> Bool
    sameDVar nId1 nId2 = getDVar nId1 == getDVar nId2
    mulOneIfAlone nId
      | DVar _ <- retrieveOp nId mp = num_ 1 |*| just nId
      | otherwise = just nId

-- | After group Dvar to groups, we aggregate result in each group
--   ((f * dx + x * dx) + (h * dy) + (t1 <.> dx1 + f1 <.> dx1)
--   --> ((f + x) * dx) + (h * dy) + ((t1 + f1) <.> dx1)
aggregateByDVar :: Transformation
aggregateByDVar =
  toRecursiveTransformation . chainModifications . map fromSubstitution $
    [ sumP (mapL (|*| y) xs) |. isDVar y ~~~~~~> sumP xs |*| y,
      sumP (mapL (|<.>| y) xs) |. isDVar y ~~~~~~> sumP xs |<.>| y
    ]

-- | Normalize each partial derivative
normalizeEachPartialDerivative :: Transformation
normalizeEachPartialDerivative exp@(mp, n) =
  case retrieveOp n mp of
    Sum ns | retrieveElementType n mp == Covector -> sumMany $ map normalizeEach ns
    MulD _ _ | retrieveElementType n mp == Covector -> normalizeEach n
    InnerProdD _ _ | retrieveElementType n mp == Covector -> normalizeEach n
    _ -> (mp, n)
  where
    normalizeEach nId =
      case retrieveOp nId mp of
        MulD partialDeriv dVar -> apply (Binary specMulD) [normalizingTransformation (mp, partialDeriv), (mp, dVar)]
        InnerProdD partialDeriv dVar -> apply (Binary specInnerProdD) [normalizingTransformation (mp, partialDeriv), (mp, dVar)]
        a -> error $ show a
