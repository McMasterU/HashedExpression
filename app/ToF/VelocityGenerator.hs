module ToF.VelocityGenerator where

import Data.Array

-- |
--
type ToFVelocity
     = ( Array (Int, Int) Double -- vX
       , Array (Int, Int) Double -- vY
        )

-- |
--
straightFlow :: (Int, Int) -> Int -> Int -> Double -> ToFVelocity
straightFlow size@(row, column) start width scalingFactor = (vX, vY)
  where
    makeVY _ j
        | j < start || j >= start + width = 0
        | otherwise =
            let ds = fromIntegral start
                dw = fromIntegral width
                dj = fromIntegral j + 0.5
                x = abs (dj - (ds + dw / 2))
             in scalingFactor * ((dw / 2) ^2 - x ^ 2)
    vX = listArray ((0, 0), (row - 1, column - 1)) $ repeat 0
    vY =
        listArray
            ((0, 0), (row - 1, column - 1))
            [makeVY i j | i <- [0 .. row - 1], j <- [0 .. column - 1]]
