{-# LANGUAGE TupleSections #-}
module Main where

import HashedInstances
import HashedDerivative
import HashedSimplify
import HashedExpression
import HashedFactor

[x1, y1, z1, u1, v1, w1] = map (var1d 4) ["X1", "Y1", "Z1", "U1", "V1", "W1"]

--e = x1 + y1 + z1
--e = shift
e = shiftScale 1 1 x1

-- TODO run tests? or anything really
main = do
    print . unOneD $ e
