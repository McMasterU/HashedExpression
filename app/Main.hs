{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.Map (empty, fromList, union)
import qualified Data.Set as Set
import HashedExpression.Derivative
import HashedExpression.Expression

import HashedExpression.Interp
import HashedNormalize
import HashedOperation
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
import Fruit.Fruit
import Graphics.EasyPlot
import HashedExpression.CollectDifferential
import HashedPlot
import HashedSolver
import HashedToC (singleExpressionCProgram)
import HashedExpression.Utils
import HashedVar
import Problems
import RecoverKSpace.RecoverKSpace
import Test.Hspec
import ToF.VelocityGenerator

main :: IO ()
main = do
    let exp =
            Expression @Scalar @R 902483289149750 $
            IM.fromList
                [ (4116, ([], Var "a"))
                , (5942, ([], Var "w"))
                , (902483289149750, ([], Sum R [5942, 1523609918601139]))
                , (1523609918601139, ([], Neg R 4116))
                ]
    showExp exp
    showExp $ exteriorDerivative allVars exp
    showExp . collectDifferentials $ exteriorDerivative allVars exp
    
    print "Helelele"
