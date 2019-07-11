{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Array
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

import Data.List (intercalate)
import Data.Maybe (fromJust)
import HashedToC (generateProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

main = do
--    let exp1 = ((negate (const 16.68869279125222)) +: (t + d + const 4.874246963013614 + f))
--    let exp2 = ((const (-12.137127243968372) +: const 0.0) + (k +: w) + (s +: o))
--    let valMaps =
--            ValMaps
--                { vm0 =
--                      fromList
--                          [ ("d", 14.84231720618179)
--                          , ("f", 27.188882137553513)
--                          , ("t", 36.13931293443671)
--                          , ("k", 27.188882137553513)
--                          , ("w", 36.13931293443671)
--                          , ("s", 27.188882137553513)
--                          , ("o", 36.13931293443671)
--                          ]
--                , vm1 = fromList []
--                , vm2 = fromList []
--                , vm3 = fromList []
--                }
--    showExp exp1
--    showExp exp2
--    showExp . simplify $ f * g
----    let valMaps = emptyVms |> withVm0 (fromList [("r", 88.3314)])
--    print $ eval valMaps (exp1 * exp2)
--    print $ eval valMaps $ simplify (exp1 * exp2)
      let g = (x+y)^2 + (x-y)^2
      showExp g
      showExp . simplify $ g
