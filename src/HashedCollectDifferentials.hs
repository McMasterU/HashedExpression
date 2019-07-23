module HashedCollectDifferentials where

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
import HashedSimplify (flattenSumProdRules, simplify)
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
            ]

-- |
--
toRecursiveCollecting :: Modification -> Transformation
toRecursiveCollecting = toTransformation . makeRecursive False

-- | Change to multiplication whenever possible, then flatten sum and product to prepare for splitCovectorProdRules
--
restructureRules :: Transformation
restructureRules =
    chain
        [ fromSubstitutionRules1 --
        , toRecursiveCollecting flattenSumProdRules
        ]
  where
    fromSubstitutionRules1 =
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
        _ -> noChange n

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
