{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (empty, fromList, union)
import qualified Data.Set as Set
import HashedDerivative
import HashedExpression
import HashedInterp
import HashedNormalize
import HashedOperation hiding (product, sum)
import qualified HashedOperation
import HashedPrettify
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
import ToF.ToF

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import Graphics.EasyPlot
import HashedCollect
import HashedPlot
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedUtils
import HashedVar
import Test.Hspec
import ToF.VelocityGenerator

sum1 :: (DimensionType d, Addable et) => [Expression d et] -> Expression d et
sum1 = fromJust . HashedOperation.sum

prod1 :: (DimensionType d, NumType et) => [Expression d et] -> Expression d et
prod1 = fromJust . HashedOperation.product

--
main = do
    let exp1 =
            Expression
                { exIndex = 904877758160803
                , exMap =
                      IM.fromList
                          [ (88964547, ([10], Var "x1"))
                          , ( 598333643278836
                            , ([10], Const (-0.17196227998152233)))
                          , ( 686007913774408
                            , ([10], Const (-6.044842432675361e-3)))
                          , ( 686196979225376
                            , ([10], Const 5.974704675973538e-2))
                          , (738408852338448, ([10], Const 0.9451022920053725))
                          , ( 765497370346384
                            , ([10], Sum R [88964547, 1711546726768938]))
                          , ( 904877758160803
                            , ( [10]
                              , Sum R
                                    [ 686007913774408
                                    , 738408852338448
                                    , 598333643278836
                                    , 765497370346384
                                    ]))
                          , (1711546726768938, ([10], Neg R 686196979225376))
                          ]
                } :: Expression One R
    let exp2 =
            Expression
                { exIndex = 1169790898199288
                , exMap =
                      IM.fromList
                          [ (83246677, ([10], Var "n1"))
                          , (83818464, ([10], Var "o1"))
                          , (86105612, ([10], Var "s1"))
                          , (761793023569293, ([10], Const 0.0))
                          , ( 800767916014208
                            , ([10], Sum R [83818464, 1739187942155526]))
                          , ( 1169790898199288
                            , ([10], Mul R [86105612, 6499321340790837]))
                          , (1739187942155526, ([10], Neg R 83246677))
                          , ( 6499321340790837
                            , ([10], ImagPart 6620409738210772))
                          , ( 6620409738210772
                            , ([10], RealImag 7766347490081547 8020278529464338))
                          , ( 6711206041745161
                            , ([10], RealImag 800767916014208 761793023569293))
                          , (7766347490081547, ([10], ReFT 6711206041745161))
                          , (8020278529464338, ([10], ImFT 6711206041745161))
                          ]
                } :: Expression One R
    let exp = exp1 <.> exp2
    showExp $ collectDifferentials . exteriorDerivative allVars $ exp
    return ()
