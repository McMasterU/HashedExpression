module HashedCollectD where

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
import HashedSimplify
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

-- | Precondition:
-- - No complex in the input (:+, xRe, xIm)
--
collectDifferentials :: Expression Zero Covector -> Expression Zero Covector
collectDifferentials = wrap . applyRules . unwrap . simplify
  where
    applyRules = multipleTimes 100 $ rulesFromSubstitution >>> id

rulesFromSubstitution :: Transformation
rulesFromSubstitution =
    chain . map (toRecursiveTransformation . fromSubstitution) . concat $
    [normalizedRules]

normalizedRules :: [Substitution]
normalizedRules =
    [ x *. y |. isScalar y ~~~~~~> x * y
    , x <.> y |. isScalar x &&. isScalar y ~~~~~~> x * y
    ]
