{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array.Unboxed as U
import HashedDerivative
import HashedExpression
import HashedFactor
import HashedInterp
import HashedSimplify

import Test.Hspec
import Test.QuickCheck hiding (scale)

-- TODO run tests? or anything really
main = hspec $ do
    describe "eval test" $ do
        specify "test here" $ do
            print "Hello world"
