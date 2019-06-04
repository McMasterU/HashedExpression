{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array.Unboxed as U
import HashedDerivative
import HashedExpression
import HashedFactor
import HashedInstances
import HashedInterp
import HashedSimplify

import Test.Hspec
import Test.QuickCheck hiding (scale)

-- TODO run tests? or anything really
main = hspec $ do
    describe "eval test" $ do
        specify "test here" $ do
            let size = 5
                x1 = var1d size "x1"
                e = shift 1 x1
            evalOneD
                (simplify e)
                (subs
                     ( []
                     , [("x1", U.listArray (0, size - 1) [1, 2, 3, 4, 5])]
                     , []
                     , []
                     , [])) `shouldBe`
                U.listArray (0, size - 1) [0, 1, 2, 3, 4]
