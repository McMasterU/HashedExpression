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

main = do
    let valMaps =
            emptyVms |>
            withVm0
                (fromList $ map (, 1) . map return $ ['a' .. 'z']) |>
            withVm1
                (fromList $
                 map (, listArray (0, 9) [1 .. 10]) . map (++ "1") . map return $
                 ['a' .. 'z'])
    let exp1 = ((t+:j)<.>(j+:y))
    let exp2 = ((((((const1d 10 (-19.72113497952217)+: const1d 10 (-5.632752111669537))^2))^2)<.>((e1+:i1)+negate((j1+:m1)))))
    print $ eval valMaps exp1 * eval valMaps exp2
    print $ eval valMaps $ simplify $ exp1 * exp2
