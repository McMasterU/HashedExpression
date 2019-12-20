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
             in scalingFactor * ((dw / 2) ^ 2 - x ^ 2)
    vX = listArray ((0, 0), (row - 1, column - 1)) $ repeat 0
    vY =
        listArray
            ((0, 0), (row - 1, column - 1))
            [makeVY i j | i <- [0 .. row - 1], j <- [0 .. column - 1]]

-- |
--
quarterCircleFlow :: (Int, Int) -> Int -> Int -> Double -> ToFVelocity
quarterCircleFlow size@(row, column) start width scalingFactor = (vX, vY)
  where
    makeVY i j =
        let y = (fromIntegral row - fromIntegral i - 0.5)
            x = -(fromIntegral column - fromIntegral j - 0.5)
            rad = sqrt $ x ^ 2 + y ^ 2
         in if rad < fromIntegral start ||
               rad >= fromIntegral start + fromIntegral width
                then (0, 0)
                else let ds = fromIntegral start
                         dw = fromIntegral width
                         l = abs (rad - (ds + dw / 2))
                         len = scalingFactor * ((dw / 2) ^ 2 - l ^ 2)
                      in (y * len / rad, -x * len / rad)
    vX =
        listArray
            ((0, 0), (row - 1, column - 1))
            [fst $ makeVY i j | i <- [0 .. row - 1], j <- [0 .. column - 1]]
    vY =
        listArray
            ((0, 0), (row - 1, column - 1))
            [snd $ makeVY i j | i <- [0 .. row - 1], j <- [0 .. column - 1]]
