{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HashedHash where

import qualified Data.IntMap.Strict as IM
import HashedExpression hiding ((+), (*))

-- |
--
class HasHash a where
    hash :: a -> Int

-- | Helper hash functions, copy from HashedExpression
--
moveBase :: Char -> Int -> Int
moveBase c hash = hash * 40591 + fromEnum c

argHash :: [Int] -> Int
argHash (arg:args) = arg + 31 * argHash args
argHash [] = 0

rehash :: Int -> [Int]
rehash x = x : [x + (241 + x * 251) * i | i <- [1 ..]]

-- | HasHash instances
--
instance HasHash Internal where
    hash (shape, node) = hash node * (1 + argHash shape)

instance HasHash ET where
    hash R = 423
    hash C = 451
    hash Covector = 269

instance HasHash Node where
    hash node =
        case node of
            Var name -> foldr moveBase 0 name
            DVar name -> foldr moveBase 1123 name
            Const num -> 919393 + foldr moveBase 0 (show num)
            -- MARK: Basics
            Sum rc args -> (1 + argHash (hash rc : args)) * 2131
            Mul rc args -> (1 + argHash (hash rc : args)) * 3343
            Neg rc arg -> (1 + argHash [hash rc, arg]) * 2293
            -- MARK: only apply to R
            Div arg1 arg2 -> (1 + argHash [arg1, arg2]) * 2621
            Sqrt arg -> (1 + argHash [arg]) * 3083
            Sin arg -> (1 + argHash [arg]) * 1009
            Cos arg -> (1 + argHash [arg]) * 1013
            Tan arg -> (1 + argHash [arg]) * 1019
            Exp arg -> (1 + argHash [arg]) * 1031
            Log arg -> (1 + argHash [arg]) * 1033
            Sinh arg -> (1 + argHash [arg]) * 1039
            Cosh arg -> (1 + argHash [arg]) * 1049
            Tanh arg -> (1 + argHash [arg]) * 1051
            Asin arg -> (1 + argHash [arg]) * 1061
            Acos arg -> (1 + argHash [arg]) * 1063
            Atan arg -> (1 + argHash [arg]) * 1069
            Asinh arg -> (1 + argHash [arg]) * 1087
            Acosh arg -> (1 + argHash [arg]) * 1091
            Atanh arg -> (1 + argHash [arg]) * 1093
            -- MARK: Complex related
            RealPart arg -> (1 + argHash [arg]) * 223
            ImagPart arg -> (1 + argHash [arg]) * 227
            RealImag arg1 arg2 -> (1 + argHash [arg1, arg2]) * 229

-- |
--
data HashOutcome
    = IsClash
    | IsDuplicate Int
    | IsNew Int
    deriving (Eq, Show, Ord)

hashOutcome :: ExpressionMap -> Internal -> Int -> HashOutcome
hashOutcome mp new newHash =
    case IM.lookup newHash mp of
        Nothing -> IsNew newHash
        Just old ->
            if old == new
                then IsDuplicate newHash
                else IsClash

-- | Where should these functions belong to?
--
addEdge :: ExpressionMap -> Internal -> (ExpressionMap, Int)
addEdge mp e =
    case dropWhile (== IsClash) . map (hashOutcome mp e) . rehash . hash $ e of
        (IsDuplicate h:_) -> (mp, h)
        (IsNew h:_) -> (IM.insert h e mp, h)
        _ -> error "addEdge everything clashed!"

fromNode :: Internal -> (ExpressionMap, Int)
fromNode e = (mp, h)
  where
    h = hash e
    mp = IM.insert h e IM.empty
