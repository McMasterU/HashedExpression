{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HashedHash where

import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import Debug.Trace (traceShowId)
import HashedExpression hiding ((*), (+), (-))

-- | modulo and radix for base-hashing
-- See https://cp-algorithms.com/string/string-hashing.html
--
modulo :: Int
modulo = 253931039382791

-- | Hash string consists of alphabet and numeric
--
radix :: Int
radix = 83

-- | Hash string
-- s[0] + s[1]⋅p + s[2]⋅p^2 + ... + s[n−1]⋅p^n−1 mod m
hashString :: String -> Int
hashString (x:xs) = ((ord x - ord '0') + radix * hashString xs) `mod` modulo
hashString [] = 0

-- | Offset the hash by number of m
-- Because each kind of node will be offset by a different offset
-- This means that 2 nodes of the same types (Var, Sum , ..) can't have the same hash
--
offsetHash :: Int -> Int -> Int
offsetHash offset hash =
    if hash < modulo
        then offset * modulo + hash
        else error "????"

rehash :: Int -> [Int]
rehash x = x : [x + (241 + x * 251) * i | i <- [1 ..]]

-- | Any string used as a separator between node ids (which are numbers)
--
separator :: String
separator = "a"

-- | HasHash instances
--
hash :: Internal -> Int
hash (shape, node) =
    let hashString' s =
            hashString $
            (intercalate separator . map show $ shape) ++ separator ++ s
     in case node of
            Var name -> offsetHash 0 . hashString' $ name
            DVar name -> offsetHash 1 . hashString' $ show name
            Const num -> offsetHash 2 . hashString' $ show num
            Sum et args ->
                offsetHash 3 . hashString' $
                show et ++ (intercalate separator . map show $ args)
            Mul et args ->
                offsetHash 4 . hashString' $
                show et ++ (intercalate separator . map show $ args)
            Power x arg ->
                offsetHash 5 . hashString' $ show x ++ "of" ++ show arg
            Neg et arg -> offsetHash 6 . hashString' $ show et ++ show arg
            Scale et arg1 arg2 ->
                offsetHash 7 . hashString' $
                show et ++ show arg1 ++ separator ++ show arg2
        -- MARK: only apply to R
            Div arg1 arg2 ->
                offsetHash 8 . hashString' $ show arg1 ++ separator ++ show arg2
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
            RealImag arg1 arg2 ->
                offsetHash 26 . hashString' $
                show arg1 ++ separator ++ show arg2
            InnerProd et arg1 arg2 ->
                offsetHash 27 . hashString' $
                show et ++ show arg1 ++ separator ++ show arg2
        -- MARK: Piecewise
            Piecewise marks arg branches ->
                offsetHash 28 . hashString' $
                (intercalate separator . map show $ marks) ++
                separator ++
                show arg ++
                separator ++ (intercalate separator . map show $ branches)
        -- MARK: Rotate
            Rotate amount arg ->
                offsetHash 29 . hashString' $
                (intercalate separator . map show $ amount) ++
                separator ++ show arg
            ReFT arg -> offsetHash 30 . hashString' $ show arg
            ImFT arg -> offsetHash 31 . hashString' $ show arg
            TwiceReFT arg -> offsetHash 32 . hashString' $ show arg
            TwiceImFT arg -> offsetHash 33 . hashString' $ show arg

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

-- |
--
addInternal :: ExpressionMap -> Internal -> (ExpressionMap, Int)
addInternal mp e =
    case dropWhile (== IsClash) . map (hashOutcome mp e) . rehash . hash $ e of
        (IsDuplicate h:_) -> (mp, h)
        (IsNew h:_) -> (IM.insert h e mp, h)
        _ -> error "addEntry everything clashed!"

fromNode :: Internal -> (ExpressionMap, Int)
fromNode e = (mp, h)
  where
    h = hash e
    mp = IM.insert h e IM.empty
