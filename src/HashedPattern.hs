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

-- | Pattern for simplification
--
type Capture = Int

-- | This reflexes Node in HashedExpression
--
data Pattern
    = PHole Capture
    | PConst Double
    | PSum [Pattern] -- element-wise sum
    | PMul [Pattern] -- multiply --> have different meanings (scale in vector space, multiplication, ...)
    | PDiv Pattern Pattern
    | PSqrt Pattern
    | PSin Pattern
    | PCos Pattern
    | PTan Pattern
    | PExp Pattern
    | PLog Pattern
    | PSinh Pattern
    | PCosh Pattern
    | PTanh Pattern
    | PAsin Pattern
    | PAcos Pattern
    | PAtan Pattern
    | PAsinh Pattern
    | PAcosh Pattern
    | PAtanh Pattern
    | PRealImag Pattern Pattern -- from real and imagine
    | PRealPart Pattern -- extract real part
    | PImagPart Pattern -- extract imaginary part
    deriving (Show, Eq, Ord)

instance AddableOp Pattern Pattern Pattern where
    (+) wh1 wh2 =  PSum [wh1, wh2]

--    (+) wh1 wh2 = WHSum [wh1, wh2]
instance MultiplyOp Pattern Pattern Pattern where
    (*) wh1 wh2 = PMul [wh1, wh2]

instance VectorSpaceOp Pattern Pattern where
    scale wh1 wh2 = PMul [wh1, wh2]

instance NumOp Pattern where
    sqrt = PSqrt
    exp = PExp
    log =  PLog
    sin = PSin
    cos =  PCos
    tan =  PTan
    asin =  PAsin
    acos =  PAcos
    atan =  PAtan
    sinh =  PSinh
    cosh =  PCosh
    tanh =  PTanh
    asinh =  PAsinh
    acosh =  PAcosh
    atanh =  PAtanh

instance ComplexRealOp Pattern Pattern where
    (+:) = PRealImag
    realPart =  PRealPart
    imagPart =  PImagPart

-- | Guarded patterns for simplification
--
data GuardedPattern =
    GP Pattern (ExpressionMap -> [(Capture, Int)] -> Bool)

-- | Helper to make pattern and replacement without condition
--
(|.~~>) :: Pattern -> Pattern -> (GuardedPattern, Pattern)
(|.~~>) pattern replacement = (GP pattern $ const (const True), replacement)

infix 0 |.~~>

[p, q, r, s, t, u, v, w, x, y, z] = map PHole [1 .. 11]

one :: Pattern
one = PConst 1

zero :: Pattern
zero = PConst 0

-- | Match an expression with a pattern, return the map between capture hole to the actual node
--
match :: (ExpressionMap, Int) -> Pattern -> Maybe [(Capture, Int)]
match (mp, n) wh =
    let recursiveAndCombine :: [Arg] -> [Pattern] -> Maybe [(Capture, Int)]
        recursiveAndCombine args whs
            | length args == length whs
            , let subMatches = zipWith match (map (mp, ) args) whs
            , all isJust subMatches = Just . concat . catMaybes $ subMatches
            | otherwise = Nothing
     in case (retrieveNode n mp, wh) of
            (_, PHole capture) -> Just [(capture, n)]
            (Const c, PConst pc) -> Just []
--            (node, PatternOp patternOp subPatterns)
--                | sameOp node patternOp ->
--                    recursiveAndCombine (args node) subPatterns

lookupCapture :: Capture -> [(Capture, Int)] -> Maybe Int
lookupCapture = lookup
