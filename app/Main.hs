module Main where

import Data.Array
import qualified Data.IntMap.Strict as IM
import Data.Map (fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
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

import Data.Complex (Complex(..))
import Data.List (intercalate)
import Data.Maybe (fromJust)
import HashedToC (generateProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

main = do
    let sum = fromJust . HashedOperation.sum
    let kaka = (sum [d +: w, z +: a, const (0) +: const (0.0)]) ^ 2
    let valMaps =
            ValMaps
                { vm0 =
                      fromList
                          [ ("a", 1)
                          , ("d", 1)
                          , ("e", 1)
                          , ("p", 1)
                          , ("w", 1)
                          , ("z", 2)
                          ]
                , vm1 = fromList []
                , vm2 = fromList []
                , vm3 = fromList []
                }
    let simplified = simplify kaka
    showExp simplified
    print $ eval valMaps kaka
    print $ eval valMaps $ simplify kaka
