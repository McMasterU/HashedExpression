-- |
-- Module      :  HashedExpression.Internal.Hash
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- Every 'NodeID' in a 'ExpressionMap' is computed using a generated hash value. This module contains all the funcitonality necessary to compute
-- hash values and check for collisions. Currently, we use a String Hashing based algorithm for our hash function, for details see here
--  https://cp-algorithms.com/string/string-hashing.html
module HashedExpression.Internal.Hash
  ( hash,
    addNode,
    fromNode,
  )
where

import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Debug.Trace (traceShowId)
import HashedExpression.Internal.Expression

-- | hardcoded modulos used in hash function (i.e 'hashString')
modulo :: Int
modulo = 253931039382791

-- | hardcoded radix used in hash function (i.e 'hashString')
radix :: Int
radix = 83

-- | Hash string
-- s[0] + s[1]⋅p + s[2]⋅p^2 + ... + s[n−1]⋅p^n−1 mod m
hashString :: String -> Int
hashString (x : xs) = ((ord x - ord '0') + radix * hashString xs) `mod` modulo
hashString [] = 0

-- | Offset the hash by number of m
-- Because each kind of node will be offset by a different offset
-- This means that 2 nodes of the same types (Var, Sum , ..) can't have the same hash
offsetHash :: Int -> Int -> Int
offsetHash offset hash =
  if hash < modulo
    then offset * modulo + hash
    else error "????"

rehash :: Int -> [Int]
rehash x = x : [x + (241 + x * 251) * i | i <- [1 ..]]

-- | Any string used as a separator between node ids (which are numbers)
separator :: String
separator = "a"

-- | Compute a hash value for a given 'Node' (don't use this directly for identify a 'Node', instead use 'addNode' to generate a
--   specific 'NodeID')
hash :: Node -> Int
hash (shape, et, node) =
  let hashString' s =
        hashString $
          (intercalate separator . map show $ shape) ++ separator ++ show et ++ separator ++ s
   in case node of
        Var name -> offsetHash 0 . hashString' $ name
        Const num -> offsetHash 2 . hashString' $ show num
        Sum args -> offsetHash 3 . hashString' $ intercalate separator . map show $ args
        Mul args -> offsetHash 4 . hashString' $ intercalate separator . map show $ args
        Power x arg -> offsetHash 5 . hashString' $ show x ++ "of" ++ show arg
        Neg arg -> offsetHash 6 . hashString' $ show arg
        Scale arg1 arg2 -> offsetHash 7 . hashString' $ show arg1 ++ separator ++ show arg2
        -- MARK: only apply to R
        Div arg1 arg2 -> offsetHash 8 . hashString' $ show arg1 ++ separator ++ show arg2
        Sqrt arg -> offsetHash 9 . hashString' $ show arg
        Sin arg -> offsetHash 10 . hashString' $ show arg
        Cos arg -> offsetHash 11 . hashString' $ show arg
        Tan arg -> offsetHash 12 . hashString' $ show arg
        Exp arg -> offsetHash 13 . hashString' $ show arg
        Log arg -> offsetHash 14 . hashString' $ show arg
        Sinh arg -> offsetHash 15 . hashString' $ show arg
        Cosh arg -> offsetHash 16 . hashString' $ show arg
        Tanh arg -> offsetHash 17 . hashString' $ show arg
        Asin arg -> offsetHash 18 . hashString' $ show arg
        Acos arg -> offsetHash 19 . hashString' $ show arg
        Atan arg -> offsetHash 20 . hashString' $ show arg
        Asinh arg -> offsetHash 21 . hashString' $ show arg
        Acosh arg -> offsetHash 22 . hashString' $ show arg
        Atanh arg -> offsetHash 23 . hashString' $ show arg
        -- MARK: Complex related
        RealPart arg -> offsetHash 24 . hashString' $ show arg
        ImagPart arg -> offsetHash 25 . hashString' $ show arg
        RealImag arg1 arg2 -> offsetHash 26 . hashString' $ show arg1 ++ separator ++ show arg2
        --
        InnerProd arg1 arg2 -> offsetHash 27 . hashString' $ show arg1 ++ separator ++ show arg2
        Piecewise marks arg branches ->
          offsetHash 28 . hashString' $
            (intercalate separator . map show $ marks)
              ++ separator
              ++ show arg
              ++ separator
              ++ (intercalate separator . map show $ branches)
        Rotate amount arg -> offsetHash 29 . hashString' $ (intercalate separator . map show $ amount) ++ separator ++ show arg
        ReFT arg -> offsetHash 30 . hashString' $ show arg
        ImFT arg -> offsetHash 31 . hashString' $ show arg
        TwiceReFT arg -> offsetHash 32 . hashString' $ show arg
        TwiceImFT arg -> offsetHash 33 . hashString' $ show arg
        -- Mark: Covector
        DVar name -> offsetHash 1 . hashString' $ show name
        DZero -> offsetHash 34 . hashString' $ "dzero"
        MulD arg1 arg2 -> offsetHash 35 . hashString' $ show arg1 ++ separator ++ show arg2
        ScaleD arg1 arg2 -> offsetHash 36 . hashString' $ show arg1 ++ separator ++ show arg2
        DScale arg1 arg2 -> offsetHash 37 . hashString' $ show arg1 ++ separator ++ show arg2
        InnerProdD arg1 arg2 -> offsetHash 38 . hashString' $ show arg1 ++ separator ++ show arg2

-- | Check the outcome of a generated hash value
data HashOutcome
  = -- | clashes with an old hash
    IsClash
  | -- | duplicate of a hash (able to reuse)
    IsDuplicate Int
  | -- | new hash value
    IsNew Int
  deriving (Eq, Show, Ord)

hashOutcome :: ExpressionMap -> Node -> Int -> HashOutcome
hashOutcome mp new newHash =
  case IM.lookup newHash mp of
    Nothing -> IsNew newHash
    Just old ->
      if old == new
        then IsDuplicate newHash
        else IsClash

-- | Compute a 'NodeID' using a hash mapping (computed with 'hash')
addNode :: ExpressionMap -> Node -> (ExpressionMap, NodeID)
addNode mp e =
  case dropWhile (== IsClash) . map (hashOutcome mp e) . rehash . hash $ e of
    (IsDuplicate h : _) -> (mp, h)
    (IsNew h : _) -> (IM.insert h e mp, h)
    _ -> error "addEntry everything clashed!"

-- | Create a unique 'NodeID' with an accompanying (singleton) 'ExpressionMap' from a standalone 'Node'
fromNode :: Node -> Expression d et
fromNode e = Expression h mp
  where
    h = hash e
    mp = IM.insert h e IM.empty
