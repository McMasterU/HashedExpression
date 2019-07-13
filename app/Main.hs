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

main
--    let sum = fromJust . HashedOperation.sum
 = do
    let exp = ((((const1d 10 (2)) + t1)) ^ 2)
    let valMaps =
            ValMaps
                { vm0 = fromList []
                , vm1 =
                      fromList
                          [ ( "T1"
                            , array
                                  (0, 9)
                                  [ (0, 1)
                                  , (1, 1)
                                  , (2, 1)
                                  , (3, 1)
                                  , (4, 1)
                                  , (5, 1)
                                  , (6, 1)
                                  , (7, 1)
                                  , (8, 1)
                                  , (9, 1)
                                  ])
                          ]
                , vm2 = fromList []
                , vm3 = fromList []
                }
    showExpDebug $ simplify exp
    print $ eval valMaps exp
    print $ eval valMaps $ simplify exp
