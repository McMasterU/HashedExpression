{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------
-- |
-- (c) 2014 Christopher Kumar Anand
-- Helper functions/instances to make pattern gaurds involving Expressions
-- easier to read.
--
-------------------------------------------------------------------------------
module HashedPattern where

import Data.Map (Map, union)
import qualified Data.Map.Strict as Map
import Data.Maybe
import HashedExpression
import HashedNode
import HashedUtils
import Prelude hiding
    ( (*)
    , (+)
    , (-)
    , (/)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , cos
    , cosh
    , exp
    , negate
    , sin
    , sinh
    , tan
    , tanh
    )

-- | Pattern for simplification
--
type Capture = Int

type ListCapture = Int

data PatternList =
    PListHole [Pattern -> Pattern] ListCapture

-- |
--
data Pattern
    = PHole Capture
    -- MARK: For list capture
    | PSumList PatternList
     -- Ref to a node in the expression
    | PRef Int
    -- MARK: Reflex Node in HashedExpression
    | PConst Double
    | PSum [Pattern]
    | PMul [Pattern]
    | PNeg Pattern
    | PScale (Pattern) Pattern
    | PDiv (Pattern) Pattern
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

instance AddableOp (Pattern) where
    (+) wh1 wh2 = PSum [wh1, wh2]
    negate = PNeg

instance MultiplyOp (Pattern) (Pattern) (Pattern) where
    (*) wh1 wh2 = PMul [wh1, wh2]

instance MultiplyOp (Pattern) (PatternList) (PatternList) where
    (*) wh1 (PListHole f listCapture) = PListHole ((wh1 *) : f) listCapture

instance MultiplyOp (PatternList) (Pattern) (PatternList) where
    (*) (PListHole f listCapture) wh2 = PListHole ((* wh2) : f) listCapture

instance VectorSpaceOp (Pattern) (Pattern) where
    scale = PScale

instance VectorSpaceOp (Pattern) (PatternList) where
    scale wh1 (PListHole f listCapture) =
        PListHole ((wh1 `scale`) : f) listCapture

instance NumOp (Pattern) where
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

instance ComplexRealOp (Pattern) (Pattern) where
    (+:) = PRealImag
    xRe = PRealPart
    xIm = PImagPart

instance InnerProductSpaceOp (Pattern) (Pattern) (Pattern) where
    (<.>) = PInnerProd

instance InnerProductSpaceOp (Pattern) (PatternList) (PatternList) where
    (<.>) wh1 (PListHole f listCapture) = PListHole ((wh1 <.>) : f) listCapture

instance InnerProductSpaceOp (PatternList) (Pattern) (PatternList) where
    (<.>) (PListHole f listCapture) wh2 = PListHole ((<.> wh2) : f) listCapture

-- | Guarded patterns for simplification
--
data GuardedPattern =
    GP (Pattern) (ExpressionMap -> Match -> Bool)

-- | Helper to make pattern and replacement without condition
--
(|.~~>) :: Pattern -> Pattern -> (GuardedPattern, Pattern)
(|.~~>) pattern replacement = (GP pattern $ const (const True), replacement)

infix 0 |.~~>, ~~>

(~~>) :: GuardedPattern -> Pattern -> (GuardedPattern, Pattern)
(~~>) gPattern replacement = (gPattern, replacement)

(|.) :: Pattern -> Condition -> GuardedPattern
(|.) pattern condition = GP pattern $ condition pattern

infixl 1 |.

type Condition = Pattern -> ExpressionMap -> Match -> Bool

-- |
--
[p, q, r, s, t, u, v, w, x, y, z] = map PHole [1 .. 11]

one :: Pattern
one = PConst 1

zero :: Pattern
zero = PConst 0

each :: PatternList
each = PListHole [] 1

sum :: PatternList -> Pattern
sum = PSumList

-- | Matches all nodes in the expression to see if they all match the PatternList, if they match, return
-- the inner actual node
-- e.g: matchList [a + (b * x), a + (b * y), a + (b * z)] (PatternList: (a + (b * _)) ---> Just [x, y, z]
--      matchList [x, y, z, t] (PatternList: (_)) = Just [x, y, z, t]
--      matchList [1 + x, 2 + x, y + x] (PatternList: (a + _)) = Nothing (not the same for all)
--
matchList :: ExpressionMap -> [Int] -> PatternList -> Maybe [Int]
matchList mp ns (PListHole fs listCapture)
    | all isJust maybeSubMatches
    , let subMatches = catMaybes maybeSubMatches
    , allEqual $ map otherCaptures subMatches = Just $ nIds subMatches
    | otherwise = Nothing
  where
    uniqueCapture = minBound :: Int
    eachHole = PHole uniqueCapture
    pt = foldr ($) eachHole fs
    matchOne nId = match (mp, nId) pt
    maybeSubMatches = map matchOne ns
    otherCaptures = Map.delete uniqueCapture . fst
    nIds subMatches =
        catMaybes . map (Map.lookup uniqueCapture . fst) $ subMatches

type Match = (Map Capture Int, Map ListCapture [Int])

-- | Match an expression with a pattern, return the map between capture hole to the actual node
-- e.g: match (Expression: (a(3243) + b(32521)) (PatternNormal:(x(1) + y(2)) --> ({1 -> 3243, 2 -> 32521}, {})
--      match (Expression sum(a(3243), b(32521), c(21321)) (PatternNormal:(sum(each(1))) --> ({}, {1 -> [3243, 32521, 21321]})
match :: (ExpressionMap, Int) -> Pattern -> Maybe Match
match (mp, n) wh =
    let unionBoth (x1, y1) (x2, y2) = (x1 `union` x2, y1 `union` y2)
        catMatch = foldl unionBoth (Map.empty, Map.empty)
        recursiveAndCombine :: [Arg] -> [Pattern] -> Maybe Match
        recursiveAndCombine args whs
            | length args == length whs
            , let subMatches = zipWith match (map (mp, ) args) whs
            , all isJust subMatches = Just . catMatch . catMaybes $ subMatches
            | otherwise = Nothing
     in case (retrieveNode n mp, wh) of
            (_, PHole capture) -> Just (Map.fromList [(capture, n)], Map.empty)
            (Const c, PConst whc)
                | c == whc -> Just (Map.empty, Map.empty)
            (Sum _ args, PSumList pl@(PListHole fs listCapture))
                | Just innerArgs <- matchList mp args pl ->
                    Just (Map.empty, Map.fromList [(listCapture, innerArgs)])
                | otherwise -> Nothing
            (Sum _ args, PSum whs) -> recursiveAndCombine args whs
            (Mul _ args, PMul whs) -> recursiveAndCombine args whs
            (Neg _ arg, PNeg whs) -> recursiveAndCombine [arg] [wh]
            (Scale _ arg1 arg2, PScale wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
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
            (InnerProd _ arg1 arg2, PInnerProd wh1 wh2) ->
                recursiveAndCombine [arg1, arg2] [wh1, wh2]
            _ -> Nothing
