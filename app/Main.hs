module Main where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedNode
import HashedInterp
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
import HashedSimplify
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

import Control.DeepSeq (deepseq)
import Data.Complex (Complex(..))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import HashedToC (generateProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)
import HashedHash

main = do
--    let exp1 = const 72.46780253629652 +: const 0.0
--    let exp2 = ((o +: o) <.> (const 33.75786494529548 +: const 0.0)) ^ 2
--    let valMaps =
--            ValMaps
--                {vm0 = fromList [("o", -77.19717883240776)], vm1 = fromList [], vm2 = fromList [], vm3 = fromList []}
--    let e = simplify $ exp1 * exp2
--    showAllEntries exp1
--    showAllEntries exp2
--    showExp exp1
--    showExp exp2
--    showExp e
--    print $ eval valMaps exp1 * eval valMaps exp2
--    print $ eval valMaps $ simplify $ exp1 * exp2
    let x = 0.0 :: Double
    let y = (-5.960464477539063e-8)

    print $ 5.960464477539063e-8 < 0.000001
    print $ x ~= y


