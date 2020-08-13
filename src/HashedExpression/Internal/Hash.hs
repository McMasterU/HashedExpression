{-# OPTIONS_GHC -Wincomplete-patterns #-}

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
    fromNodeUnwrapped,
    hashNode,
    checkHashFromMap,
    checkHashFromMaps,
    CheckHash,
    HashOutcome (..),
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
-- nodes of the different constructors (Var, Sum , ..) can't have the same hash
offsetHash :: Int -> Int -> Int
offsetHash offset hash =
  if hash < modulo
    then offset * modulo + hash
    else error "????"

-- | Any string used as a separator between node ids (which are numbers)
separator :: String
separator = "a"

toStringHash :: DimSelector -> String
toStringHash (At i) = "at" ++ show i
toStringHash (Range b e st) = "range" ++ show b ++ separator ++ show e ++ separator ++ show st

-- | Compute a hash value for a given 'Node' and number of rehash
hash :: Node -> Int -> Int
hash (shape, et, node) rehashNum =
  let hashString' s =
        hashString $
          (intercalate separator . map show $ shape) ++ separator ++ show et ++ separator ++ s ++ concat (replicate rehashNum "x")
   in case node of
        Var name -> offsetHash 0 . hashString' $ name
        Param name -> offsetHash 1 . hashString' $ name
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
        FT arg -> offsetHash 30 . hashString' $ show arg
        IFT arg -> offsetHash 31 . hashString' $ show arg
        Project ss arg -> offsetHash 32 . hashString' $ (intercalate separator . map toStringHash $ ss) ++ separator ++ show arg
        Inject ss sub base -> offsetHash 33 . hashString' $ (intercalate separator . map toStringHash $ ss) ++ separator ++ show sub ++ show base
        -------------------------------------------------------------------------------
        Conjugate arg -> offsetHash 37 . hashString' $ show arg
        -- Mark: Covector
        DVar name -> offsetHash 37 . hashString' $ show name
        DZero -> offsetHash 38 . hashString' $ "dzero"
        MulD arg1 arg2 -> offsetHash 39 . hashString' $ show arg1 ++ separator ++ show arg2
        ScaleD arg1 arg2 -> offsetHash 40 . hashString' $ show arg1 ++ separator ++ show arg2
        DScale arg1 arg2 -> offsetHash 41 . hashString' $ show arg1 ++ separator ++ show arg2
        InnerProdD arg1 arg2 -> offsetHash 42 . hashString' $ show arg1 ++ separator ++ show arg2

-- | Check the outcome of a generated hash value
data HashOutcome
  = -- | clashes with an old hash
    IsClash
  | -- | new hash value
    IsOk NodeID
  deriving (Eq, Show, Ord)

type CheckHash = Node -> NodeID -> HashOutcome

-- |
-- | IsOk if doesn't collide with the provided expression map
--   IsClash otherwise
checkHashFromMap :: ExpressionMap -> CheckHash
checkHashFromMap mp node nID =
  case IM.lookup nID mp of
    Nothing -> IsOk nID
    Just existingNode | existingNode == node -> IsOk nID
    _ -> IsClash

-- | IsOk if doesn't collide with any of provided expression map
--   IsClash otherwise
checkHashFromMaps :: [ExpressionMap] -> CheckHash
checkHashFromMaps [] node nID = IsOk nID
checkHashFromMaps (mp : mps) node nID = case checkHashFromMap mp node nID of
  IsClash -> IsClash
  IsOk _ -> checkHashFromMaps mps node nID

-- |
hashNode :: CheckHash -> Node -> NodeID
hashNode checkHash e =
  case dropWhile (== IsClash) . map (checkHash e . hash e) $ [0 .. 1000] of
    (IsOk h : _) -> h
    _ -> error "hashNode everything clashed!"

-- | Compute a 'NodeID' using a hash mapping (computed with 'hash')
addNode :: ExpressionMap -> Node -> (ExpressionMap, NodeID)
addNode mp e =
  let checkHash = checkHashFromMap mp
      h = hashNode checkHash e
   in (IM.insert h e mp, h)

-- | Create an Expression from a standalone 'Node'
fromNode :: Node -> Expression d et
fromNode e = Expression nID mp
  where
    (mp, nID) = addNode IM.empty e

-- | Create an unwrapped Expresion from a standalone 'Node'
fromNodeUnwrapped :: Node -> (ExpressionMap, NodeID)
fromNodeUnwrapped = addNode IM.empty
