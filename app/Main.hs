{-# LANGUAGE DeriveFunctor #-}

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
    let exp1 =
            ((r +: u) + (i +: l) +
             negate ((const (17.98534460707952) +: const (0.0))))
    let exp2 = ((t +: k) + (d +: w) + (h +: s))
    let kaka =
            Expression
                { exIndex = -2548377210763299520
                , exMap =
                      IM.fromList
                          [ ( -3234846063262403967
                            , ([], Sum C [867910, 716770, 3130068329764892197]))
                          , ( -2548377210763299520
                            , ([], Power 2 (-3234846063262403967)))
                          , (97, ([], Var "a"))
                          , (100, ([], Var "d"))
                          , (119, ([], Var "w"))
                          , (122, ([], Var "z"))
                          , (716770, ([], RealImag 122 97))
                          , (867910, ([], RealImag 100 119))
                          , (79088992115, ([], Const 0.0))
                          , ( 3130068329764892197
                            , ([], RealImag 4444106685288363347 79088992115))
                          , ( 4444106685288363347
                            , ([], Const (-13.125787705372137)))
                          ]
                } :: Expression Zero C
--    print kaka
--    putStrLn "--------KAKA"
--    showExp kaka
--    showExp . simplify $ kaka
--    let kiki = (((d+:w)+(z+:a)+(const (-13.125787705372137)+:const (0.0)))^2)
--    putStrLn "--------KIKI"
----    print kiki
--    showExp kiki
--    showExp . simplify $ kiki
    let sum = fromJust . HashedOperation.sum
    let kaka = (sum [(d+:w), (z+:a), (const (0)+:const (0.0))])^2
    let valMaps = ValMaps {vm0 = fromList [("a",1),("d",1),("e",1),("p",1),("w",1),("z",2)], vm1 = fromList [], vm2 = fromList [], vm3 = fromList []}
    let simplified = simplify kaka
    showExp simplified
    print $ eval valMaps kaka
    print $ eval valMaps $ simplify kaka

--    showExp $ simplify kaka
--    showExp $ simplify $ (exp1) * (simplify exp2)
--    let allSimplify = map (\_ -> simplify $ exp1 * exp2) [1..1000]
--    print $ allEqual allSimplify
--    let expMul =
--            ((((r * x) + (const (-1.8329084829569435) * x) + (const (-1.0) *. (c * f))) +:
--              ((f * p) + (k * r) + (const (-1.8329084829569435) * k))) +
--             (((g * r) + (const (-1.8329084829569435) * g) + (const (-1.0) *. (f * w))) +:
--              ((f * g) + (r * w) + (const (-1.8329084829569435) * w))) +
--             (((p * r) + (const (-1.8329084829569435) * p) + (const (-1.0) *. (f * k))) +:
--              ((f * p) + (k * r) + (const (-1.8329084829569435) * k))))
--    let valMaps =
--            ValMaps
--                { vm0 =
--                      fromList
--                          [ ("c", -42.996699908420105)
--                          , ("f", 32.962335255376956)
--                          , ("g", 2.278616799902468)
--                          , ("k", 15.932658947705567)
--                          , ("p", -45.32604905943502)
--                          , ("r", 66.23122144783572)
--                          , ("w", -1.1627731148202438)
--                          , ("x", -28.02201697062796)
--                          ]
--                , vm1 = fromList []
--                , vm2 = fromList []
--                , vm3 = fromList []
--                }
--    showExp exp1
--    showExp exp2
--    showExp . simplify $ exp1 * exp2
--    let rhs = eval valMaps $ simplify $ expMul
--    print rhs
--    print $ eval valMaps exp1 * eval valMaps exp2
