{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

-------------------------------------------------------------------------------
-- |
-- (c) 2014 Christopher Kumar Anand
-- Helper functions/instances to make pattern gaurds involving Expressions
-- easier to read.
--
-------------------------------------------------------------------------------
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
    | PSum [Pattern]
    | PMul [Pattern]
    | PNeg Pattern
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
    | PRealImag Pattern Pattern
    | PRealPart Pattern
    | PImagPart Pattern
    | PInnerProd Pattern Pattern
    deriving (Show, Eq, Ord)

instance AddableOp Pattern where
    (+) wh1 wh2 = PSum [wh1, wh2]
    negate = PNeg

--    (+) wh1 wh2 = WHSum [wh1, wh2]
instance MultiplyOp Pattern where
    (*) wh1 wh2 = PMul [wh1, wh2]

instance VectorSpaceOp Pattern Pattern where
    scale wh1 wh2 = PMul [wh1, wh2]

instance NumOp Pattern where
    sqrt = PSqrt
    exp = PExp
    log = PLog
    sin = PSin
    cos = PCos
    tan = PTan
    asin = PAsin
    acos = PAcos
    atan = PAtan
    sinh = PSinh
    cosh = PCosh
    tanh = PTanh
    asinh = PAsinh
    acosh = PAcosh
    atanh = PAtanh
    (/) = PDiv

instance ComplexRealOp Pattern Pattern where
    (+:) = PRealImag
    xRe = PRealPart
    xIm = PImagPart

instance InnerProductSpaceOp Pattern Pattern where
    (<.>) = PInnerProd

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
            (Const c, PConst whc)
                | c == whc -> Just []
            (Sum _ args, PSum whs) -> recursiveAndCombine args whs
            (Mul _ args, PMul whs) -> recursiveAndCombine args whs
            (Neg _ arg, PNeg whs) -> recursiveAndCombine [arg] [wh]
            (Div arg1 arg2, PDiv wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            (Sqrt arg, PSqrt wh) -> recursiveAndCombine [arg] [wh]
            (Sin arg, PSin wh) -> recursiveAndCombine [arg] [wh]
            (Cos arg, PCos wh) -> recursiveAndCombine [arg] [wh]
            (Tan arg, PTan wh) -> recursiveAndCombine [arg] [wh]
            (Exp arg, PExp wh) -> recursiveAndCombine [arg] [wh]
            (Log arg, PLog wh) -> recursiveAndCombine [arg] [wh]
            (Sinh arg, PSinh wh) -> recursiveAndCombine [arg] [wh]
            (Cosh arg, PCosh wh) -> recursiveAndCombine [arg] [wh]
            (Tanh arg, PTanh wh) -> recursiveAndCombine [arg] [wh]
            (Asin arg, PAsin wh) -> recursiveAndCombine [arg] [wh]
            (Acos arg, PAcos wh) -> recursiveAndCombine [arg] [wh]
            (Atan arg, PAtan wh) -> recursiveAndCombine [arg] [wh]
            (Asinh arg, PAsinh wh) -> recursiveAndCombine [arg] [wh]
            (Acosh arg, PAcosh wh) -> recursiveAndCombine [arg] [wh]
            (Atanh arg, PAtanh wh) -> recursiveAndCombine [arg] [wh]
            (RealImag arg1 arg2, PRealImag wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            (RealPart arg, PRealPart wh) -> recursiveAndCombine [arg] [wh]
            (ImagPart arg, PImagPart wh) -> recursiveAndCombine [arg] [wh]
            (InnerProd _ arg1 arg2, PInnerProd wh1 wh2) -> recursiveAndCombine [arg1, arg2] [wh1, wh2]
            _ -> Nothing

lookupCapture :: Capture -> [(Capture, Int)] -> Maybe Int
lookupCapture = lookup
