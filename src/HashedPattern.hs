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

module HashedPattern where

import qualified Data.IntMap as I
import qualified Data.List as L
import Data.Maybe
import HashedExpression
import HashedNode
import HashedUtils

type Capture = Int

data Pattern
    = PatternHole Capture
    | PatternConst Double
    | PatternOp Node [Pattern]
    deriving (Show, Eq, Ord)

instance AddableOp Pattern Pattern Pattern where
    (+) wh1 wh2 = PatternOp opSum [wh1, wh2]

--    (+) wh1 wh2 = WHSum [wh1, wh2]
instance MultiplyOp Pattern Pattern Pattern where
    (*) wh1 wh2 = PatternOp opMul [wh1, wh2]

instance VectorSpaceOp Pattern Pattern where
    scale wh1 wh2 = PatternOp opMul [wh1, wh2]

instance NumOp Pattern where
    sqrt wh = PatternOp opSqrt [wh]
    exp wh = PatternOp opExp [wh]
    log wh = PatternOp opLog [wh]
    --
    sin wh = PatternOp opSin [wh]
    cos wh = PatternOp opCos [wh]
    tan wh = PatternOp opTan [wh]
    asin wh = PatternOp opAsin [wh]
    acos wh = PatternOp opAcos [wh]
    atan wh = PatternOp opAtan [wh]
    sinh wh = PatternOp opSinh [wh]
    cosh wh = PatternOp opCosh [wh]
    tanh wh = PatternOp opTanh [wh]
    asinh wh = PatternOp opAsinh [wh]
    acosh wh = PatternOp opAcosh [wh]
    atanh wh = PatternOp opAtanh [wh]

instance ComplexRealOp Pattern Pattern where
    (+:) wh1 wh2 = PatternOp opRealImag [wh1, wh2]
    realPart wh = PatternOp opRealPart [wh]
    imagPart wh = PatternOp opImagPart [wh]

data GuardedPattern =
    GP Pattern (ExpressionMap -> [(Capture, Int)] -> Bool)

(|.~~>) :: Pattern -> Pattern -> (GuardedPattern, Pattern)
(|.~~>) pattern replacement = (GP pattern $ const (const True), replacement)

infix 0 |.~~>

[p, q, r, s, t, u, v, w, x, y, z] = map PatternHole [1 .. 11]

one :: Pattern
one = PatternConst 1

zero :: Pattern
zero = PatternConst 0

match :: (ExpressionMap, Int) -> Pattern -> Maybe [(Capture, Int)]
match (mp, n) wh =
    let recursiveAndCombine :: [Arg] -> [Pattern] -> Maybe [(Capture, Int)]
        recursiveAndCombine args whs
            | length args == length whs
            , let subMatches = zipWith match (map (mp, ) args) whs
            , all isJust subMatches = Just . concat . catMaybes $ subMatches
            | otherwise = Nothing
     in case (retrieveNode n mp, wh) of
            (_, PatternHole capture) -> Just [(capture, n)]
            (Const c, PatternConst pc) -> Just []
            (node, PatternOp patternOp subPatterns)
                | sameOp node patternOp ->
                    recursiveAndCombine (args node) subPatterns
