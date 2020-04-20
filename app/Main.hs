{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Array
import Data.Complex
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Data.Map (empty, fromList, union)
import Data.Maybe (fromJust)
import Data.STRef.Strict
import qualified Data.Set as Set
import Graphics.EasyPlot
import HashedExpression
import HashedExpression.Derivative
import HashedExpression.Interp
import HashedExpression.Operation
import qualified HashedExpression.Operation
import HashedExpression.Prettify
import Prelude hiding
  ( (*),
    (+),
    (-),
    (/),
    (^),
    acos,
    acosh,
    asin,
    asinh,
    atan,
    atanh,
    const,
    const,
    cos,
    cosh,
    exp,
    log,
    negate,
    product,
    sin,
    sinh,
    sqrt,
    sum,
    tan,
    tanh,
  )

main :: IO ()
main = do
  print "hello world"
