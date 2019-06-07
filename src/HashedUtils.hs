module HashedUtils where

import qualified Data.Complex as DC

-- | Check if all elements of the list is equal
--
allEqual :: (Eq a) => [a] -> Bool
allEqual xs = and $ zipWith (==) (safeTail xs) xs
  where
    safeTail [] = []
    safeTail (x:xs) = xs

-- | Check if xs is a prefix of ys
--
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix xs ys = any ((== xs) . flip take ys) [0 .. length ys]

-- | Check if one list is prefix of other
--
onePrefixOther :: (Eq a) => [a] -> [a] -> Bool
onePrefixOther xs ys = isPrefix xs ys || isPrefix ys xs

-- |
--
fromR :: Double -> DC.Complex Double
fromR x = x DC.:+ 0
