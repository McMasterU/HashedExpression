{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

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

import Data.List (intercalate)
import Data.Maybe (fromJust)
import HashedToC (generateProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import Test.QuickCheck hiding (scale)

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.sum

main
--    let valMaps =
--            emptyVms |>
--            withVm0 (fromList $ map (, 1) . map return $ ['a' .. 'z']) |>
--            withVm1
--                (fromList $
--                 map (, listArray (0, 9) [1 .. 10]) . map (++ "1") . map return $
--                 ['a' .. 'z'])
--    let exp = (((const1d 10 (17.896497401304458))+negate(xIm((p*.(v1+:g1)))))+j1)
--    showExpDebug exp
--    showExpDebug $ simplify exp
--    print $ eval valMaps exp
--    print $ eval valMaps (simplify exp)
 = do
    let exp1 = ((one +: c) <.> ((one +: one) *. (negate (((one +: zero) *. (one +: zero))))))
    showExp $ simplify exp1
    let valMaps =
            ValMaps
                { vm0 =
                      fromList
                          [ ("c", 1)
                          , ("f", 1)
                          , ("g", 1)
                          , ("i", 1)
                          , ("j", 1)
                          , ("k", 1)
                          , ("m", 1)
                          , ("n", 1)
                          , ("s", 1)
                          ]
                , vm1 = fromList []
                , vm2 = fromList []
                , vm3 = fromList []
                }
    print $ eval valMaps exp1
    print $ eval valMaps $ simplify $ exp1
