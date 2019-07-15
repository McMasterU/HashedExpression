{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Array
import Data.IntMap.Strict as IM
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
    let exp =
            Expression
                { exIndex = 27810492723
                , exMap =
                      IM.fromList
                          [ (75813446, ([10], Var "a1"))
                          , (77528807, ([10], Var "d1"))
                          , (80387742, ([10], Var "i1"))
                          , (83246677, ([10], Var "n1"))
                          , (84962038, ([10], Var "q1"))
                          , (86105612, ([10], Var "s1"))
                          , (86677399, ([10], Var "t1"))
                          , (88964547, ([10], Var "x1"))
                          , (2160252885, ([10], Const (-12.89524647168872)))
                          , (2310397079, ([10], Const 6.532765799496251))
                          , (2516494445, ([10], Const 3.2629845081353395))
                          , (2520511646, ([10], Const (-14.917226786690478)))
                          , (2609460305, ([10], Const (-15.519275078429809)))
                          , (2610810685, ([10], Const 6.646634384434718))
                          , (2615416077, ([10], Const (-11.413173915778156)))
                          , (2959370896, ([10], Const 10.15161879455977))
                          , ( 3022617918
                            , ( [10]
                              , Sum C [26344797819, 26923279679, 4956427616]))
                          , ( 3627528317
                            , ([10], Sum C [26781857292, 6065579504]))
                          , ( 4421452397
                            , ( [10]
                              , Mul C [3022617918, 26381922871, 26148224098]))
                          , ( 4956427616
                            , ( [10]
                              , Mul C [26380621216, 26229666270, 26107006774]))
                          , (5895068379, ([10], Power 2 26967005760))
                          , (6065579504, ([10], Neg C 4421452397))
                          , (26107006774, ([10], RealImag 86677399 80387742))
                          , (26148224098, ([10], RealImag 77528807 83246677))
                          , ( 26229666270
                            , ([10], RealImag 2615416077 2959370896))
                          , (26344797819, ([10], RealImag 88964547 88964547))
                          , ( 26380621216
                            , ([10], RealImag 2310397079 2520511646))
                          , ( 26381922871
                            , ([10], RealImag 2609460305 2516494445))
                          , (26781857292, ([10], RealImag 75813446 84962038))
                          , (26923279679, ([10], RealImag 86105612 86677399))
                          , ( 26967005760
                            , ([10], RealImag 2610810685 2160252885))
                          , ( 27810492723
                            , ([], InnerProd C 5895068379 3627528317))
                          ]
                } :: Expression Zero C
    showExp exp
    showExpDebug exp
    showExp $ simplify exp
--    measureTime $ do
--        let exp1 = (((((n +: l)) ^ 3)) ^ 3)
--        let exp2 = ((k +: u) + (p +: j))
--        showExp $ simplify $ exp1 * exp2
