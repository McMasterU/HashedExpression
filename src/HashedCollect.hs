module HashedCollect where

import Data.Function.HT (nest)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', group, groupBy, intercalate, partition, sort)
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
import HashedSimplify (flattenSumProdRules, reduceSumProdRules, simplify)
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

-- | Precondition: (satisfied by first applying simplification)
-- - No complex in the input (:+, xRe, xIm)
-- - Scale is pushed to the outer most layer and real scalars are group together in a product
--
collectDifferentials :: Expression Zero Covector -> Expression Zero Covector
collectDifferentials = wrap . applyRules . unwrap . simplify
  where
    applyRules =
        chain
            [ restructureRules
            , toRecursiveCollecting splitCovectorProdRules
            , normalizedRules
            , toTransformation groupByDVar
            , aggregateByDVar
            , reduceAndSimplify
            ]

-- |
--
toRecursiveCollecting :: Modification -> Transformation
toRecursiveCollecting = toTransformation . makeRecursive False

-- | Change to multiplication whenever possible, then flatten sum and product to prepare for splitCovectorProdRules
-- Also move covector to the right hand side of dot product
--
restructureRules :: Transformation
restructureRules =
    multipleTimes 100 $
    chain
        [ fromSubstitutionRules --
        , toRecursiveCollecting flattenSumProdRules
        ]
  where
    fromSubstitutionRules =
        chain . map (toRecursiveCollecting . fromSubstitution) $
        [ x *. y |. isScalar y ~~~~~~> x * y
        , x <.> y |. isScalar x &&. isScalar y ~~~~~~> x * y
        , x <.> y |. isCovector x ~~~~~~> y <.> x
        ]

-- | x * y * covector * z --> (x * y * z) * covector
--
splitCovectorProdRules :: Modification
splitCovectorProdRules exp@(mp, n) =
    case retrieveNode n mp of
        Mul Covector ns ->
            let (covectorPart, realPart) =
                    partition ((== Covector) . flip retrieveElementType mp) ns
                prodRealPart = mulManyDiff mp . map noChange $ realPart
             in mulManyDiff mp $ prodRealPart : map noChange covectorPart
        x -> noChange n

-- |
--
normalizedRules :: Transformation
normalizedRules =
    chain . map (toRecursiveCollecting . fromSubstitution) $
    [ x <.> (z * y) |. isDVar y ~~~~~~> (z * x) <.> y
    , x <.> (restOfProduct ~* y) |. isDVar y ~~~~~~> (restOfProduct ~* x) <.> y
    , s * (x <.> y) |. isDVar y ~~~~~~> (s *. x) <.> y
    , s * (x * y) |. isDVar y ~~~~~~> (s * x) * y
    ]

-- | Group a sum to many sums, each sum is corresponding to a DVar, preparing for aggregateByDVar
-- (f * dx + h * dy + x * dx + t1 <.> dx1 + f1 <.> dx1) -->
--   ((f * dx + x * dx) + (h * dy) + (t1 <.> dx1 + f1 <.> dx1)
--
groupByDVar :: Modification
groupByDVar exp@(mp, n) =
    case retrieveNode n mp of
        Sum Covector ns ->
            let groups =
                    groupBy sameDVar .
                    sortWith getDVar . filter (not . isZero mp) $
                    ns
                mulOneIfAlone nId
                    | DVar _ <- retrieveNode nId mp =
                        mulManyDiff mp [diffConst [] 1, noChange nId]
                    | otherwise = noChange nId
             in sumManyDiff mp . map (sumManyDiff mp . map mulOneIfAlone) $
                groups
        _ -> noChange n
  where
    getDVar :: Int -> String
    getDVar nId
        | DVar name <- retrieveNode nId mp = name
        | Mul Covector [_, cId] <- retrieveNode nId mp
        , DVar name <- retrieveNode cId mp = name
        | InnerProd Covector _ cId <- retrieveNode nId mp
        , DVar name <- retrieveNode cId mp = name
    sameDVar :: Int -> Int -> Bool
    sameDVar nId1 nId2 = getDVar nId1 == getDVar nId2

-- | After group Dvar to groups, we aggregate result in each group
--   ((f * dx + x * dx) + (h * dy) + (t1 <.> dx1 + f1 <.> dx1)
--   --> ((f + x) * dx) + (h * dy) + ((t1 + f1) <.> dx1)
--
aggregateByDVar :: Transformation
aggregateByDVar =
    chain . map (toRecursiveCollecting . fromSubstitution) $
    [ sum (mapL (* y) xs) |. isDVar y ~~~~~~> sum xs * y
    , sum (mapL (<.> y) xs) |. isDVar y ~~~~~~> sum xs <.> y
    ]

-- | TODO: Simplify each partial differential?
--
reduceAndSimplify :: Transformation
reduceAndSimplify =
    chain
        [ multipleTimes 100 $ toRecursiveCollecting reduceSumProdRules
        , removeUnreachable
        ]
