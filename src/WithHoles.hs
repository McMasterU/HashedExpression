{-
(c) 2014 Christopher Kumar Anand

Helper functions/instances to make pattern gaurds involving Expressions easier to read.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module WithHoles where

import qualified Data.IntMap as I
import qualified Data.List as L
import Data.Maybe
import HashedExpression
import HashedUtils

type Capture = Int

data WithHoles
    = WHHole Capture
    | WHConstScalar Double
    | WHConstShape Double
    | WHSum [WithHoles]
    | WHMul [WithHoles]
    | WHDiv WithHoles WithHoles
    | WHSqrt WithHoles
    | WHSin WithHoles
    | WHCos WithHoles
    | WHTan WithHoles
    | WHExp WithHoles
    | WHLog WithHoles
    | WHSinh WithHoles
    | WHCosh WithHoles
    | WHTanh WithHoles
    | WHAsin WithHoles
    | WHAcos WithHoles
    | WHAtan WithHoles
    | WHAsinh WithHoles
    | WHAcosh WithHoles
    | WHAtanh WithHoles
    | WHRealImag WithHoles WithHoles
    | WHRealPart WithHoles
    | WHImagPart WithHoles
    deriving (Show, Eq, Ord)

instance AddableOp WithHoles WithHoles WithHoles where
    (+) wh1 wh2 = WHSum [wh1, wh2]

instance MultiplyOp WithHoles WithHoles WithHoles where
    (*) wh1 wh2 = WHMul [wh1, wh2]

instance VectorSpaceOp WithHoles WithHoles where
    scale wh1 wh2 = WHMul [wh1, wh2]

instance NumOp WithHoles where
    sqrt = WHSqrt
    exp = WHExp
    log = WHLog
    -- Trigonometric operations
    sin = WHSin
    cos = WHCos
    tan = WHTan
    asin = WHAsin
    acos = WHAcos
    atan = WHAtan
    sinh = WHSinh
    cosh = WHCosh
    tanh = WHTanh
    asinh = WHAsinh
    acosh = WHAcosh
    atanh = WHAtanh

instance ComplexRealOp WithHoles WithHoles where
    (+:) = WHRealImag
    realPart = WHRealPart
    imagPart = WHImagPart

data GuardedPattern =
    GP WithHoles (ExpressionMap -> [(Capture, Int)] -> Bool)

(|.~~>) :: WithHoles -> WithHoles -> (GuardedPattern, WithHoles)
(|.~~>) pattern replacement = (GP pattern $ const (const True), replacement)

infix 0 |.~~>

[p, q, r, s, t, u, v, w, x, y, z] = map WHHole [1 .. 11]

one :: WithHoles
one = WHConstScalar 1

zero :: WithHoles
zero = WHConstScalar 0

oneS :: WithHoles
oneS = WHConstShape 1

zeroS :: WithHoles
zeroS = WHConstShape 0

match :: (ExpressionMap, Int) -> WithHoles -> Maybe [(Capture, Int)]
match (mp, n) wh =
    let recursiveAndCombine :: [Arg] -> [WithHoles] -> Maybe [(Capture, Int)]
        recursiveAndCombine args whs
            | length args == length whs
            , let subMatches = zipWith match (map (mp, ) args) whs
            , all isJust subMatches = Just . concat . catMaybes $ subMatches
            | otherwise = Nothing
     in case (retrieveNode n mp, wh) of
            (_, WHHole capture) -> Just [(capture, n)]
            (Const c, WHConstScalar whc)
                | c == whc && retrieveShape n mp == [] -> Just []
            (Const c, WHConstShape whc)
                | c == whc -> Just []
            (Sum _ args, WHSum whs) -> recursiveAndCombine args whs
            (Mul _ args, WHMul whs) -> recursiveAndCombine args whs
            (Div arg1 arg2, WHDiv wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            (Sqrt arg, WHSqrt wh) -> recursiveAndCombine [arg] [wh]
            (Sin arg, WHSin wh) -> recursiveAndCombine [arg] [wh]
            (Cos arg, WHCos wh) -> recursiveAndCombine [arg] [wh]
            (Tan arg, WHTan wh) -> recursiveAndCombine [arg] [wh]
            (Exp arg, WHExp wh) -> recursiveAndCombine [arg] [wh]
            (Log arg, WHLog wh) -> recursiveAndCombine [arg] [wh]
            (Sinh arg, WHSinh wh) -> recursiveAndCombine [arg] [wh]
            (Cosh arg, WHCosh wh) -> recursiveAndCombine [arg] [wh]
            (Tanh arg, WHTanh wh) -> recursiveAndCombine [arg] [wh]
            (Asin arg, WHAsin wh) -> recursiveAndCombine [arg] [wh]
            (Acos arg, WHAcos wh) -> recursiveAndCombine [arg] [wh]
            (Atan arg, WHAtan wh) -> recursiveAndCombine [arg] [wh]
            (Asinh arg, WHAsinh wh) -> recursiveAndCombine [arg] [wh]
            (Acosh arg, WHAcosh wh) -> recursiveAndCombine [arg] [wh]
            (Atanh arg, WHAtanh wh) -> recursiveAndCombine [arg] [wh]
            (RealImag arg1 arg2, WHRealImag wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            (RealPart arg, WHRealPart wh) -> recursiveAndCombine [arg] [wh]
            (ImagPart arg, WHImagPart wh) -> recursiveAndCombine [arg] [wh]
            _ -> Nothing
