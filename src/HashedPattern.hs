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

data Normal

data List

-- |
--
data Pattern a where
    PHole :: Capture -> Pattern Normal
    -- MARK: For list capture
    PListHole
        :: [Pattern Normal -> Pattern Normal] -> ListCapture -> Pattern List
    PSumList :: Pattern List -> Pattern Normal
    -- Ref to a node in the expression
    PRef :: Int -> Pattern Normal
    PEach :: Capture -> Pattern Normal
    -- Reflex Node in HashedExpression
    PConst :: Double -> Pattern Normal
    PSum :: [Pattern Normal] -> Pattern Normal
    PMul :: [Pattern Normal] -> Pattern Normal
    PNeg :: Pattern Normal -> Pattern Normal
    PScale :: (Pattern Normal) -> (Pattern Normal) -> Pattern Normal
    PDiv :: (Pattern Normal) -> (Pattern Normal) -> Pattern Normal
    PSqrt :: Pattern Normal -> Pattern Normal
    PSin :: Pattern Normal -> Pattern Normal
    PCos :: Pattern Normal -> Pattern Normal
    PTan :: Pattern Normal -> Pattern Normal
    PExp :: Pattern Normal -> Pattern Normal
    PLog :: Pattern Normal -> Pattern Normal
    PSinh :: Pattern Normal -> Pattern Normal
    PCosh :: Pattern Normal -> Pattern Normal
    PTanh :: Pattern Normal -> Pattern Normal
    PAsin :: Pattern Normal -> Pattern Normal
    PAcos :: Pattern Normal -> Pattern Normal
    PAtan :: Pattern Normal -> Pattern Normal
    PAsinh :: Pattern Normal -> Pattern Normal
    PAcosh :: Pattern Normal -> Pattern Normal
    PAtanh :: Pattern Normal -> Pattern Normal
    PRealImag :: Pattern Normal -> Pattern Normal -> Pattern Normal
    PRealPart :: Pattern Normal -> Pattern Normal
    PImagPart :: Pattern Normal -> Pattern Normal
    PInnerProd :: Pattern Normal -> Pattern Normal -> Pattern Normal

instance AddableOp (Pattern Normal) where
    (+) wh1 wh2 = PSum [wh1, wh2]
    negate = PNeg

instance MultiplyOp (Pattern Normal) (Pattern Normal) (Pattern Normal) where
    (*) wh1 wh2 = PMul [wh1, wh2]

instance MultiplyOp (Pattern Normal) (Pattern List) (Pattern List) where
    (*) wh1 (PListHole f listCapture) = PListHole ((wh1 *) : f) listCapture

instance MultiplyOp (Pattern List) (Pattern Normal) (Pattern List) where
    (*) (PListHole f listCapture) wh2 = PListHole ((* wh2) : f) listCapture

instance VectorSpaceOp (Pattern Normal) (Pattern Normal) where
    scale = PScale

instance VectorSpaceOp (Pattern Normal) (Pattern List) where
    scale wh1 (PListHole f listCapture) =
        PListHole ((wh1 `scale`) : f) listCapture

instance NumOp (Pattern Normal) where
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

instance ComplexRealOp (Pattern Normal) (Pattern Normal) where
    (+:) = PRealImag
    xRe = PRealPart
    xIm = PImagPart

instance InnerProductSpaceOp (Pattern Normal) (Pattern Normal) (Pattern Normal) where
    (<.>) = PInnerProd

instance InnerProductSpaceOp (Pattern Normal) (Pattern List) (Pattern List) where
    (<.>) wh1 (PListHole f listCapture) = PListHole ((wh1 <.>) : f) listCapture

instance InnerProductSpaceOp (Pattern List) (Pattern Normal) (Pattern List) where
    (<.>) (PListHole f listCapture) wh2 = PListHole ((<.> wh2) : f) listCapture

-- | Guarded patterns for simplification
--
data GuardedPattern =
    GP (Pattern Normal) (ExpressionMap -> Match -> Bool)

-- | Helper to make pattern and replacement without condition
--
(|.~~>) :: Pattern Normal -> Pattern Normal -> (GuardedPattern, Pattern Normal)
(|.~~>) pattern replacement = (GP pattern $ const (const True), replacement)

infix 0 |.~~>

-- |
--
[p, q, r, s, t, u, v, w, x, y, z] = map PHole [1 .. 11]

one :: Pattern Normal
one = PConst 1

zero :: Pattern Normal
zero = PConst 0

each :: Pattern List
each = PListHole [] 1

sum :: Pattern List -> Pattern Normal
sum = PSumList

-- | Matches all nodes in the expression to see if they all match the pattern list, if they match, return
-- the inner actual node
-- e.g: matchList [a + (b * x), a + (b * y), a + (b * z)] (PatternList: (a + (b * _)) ---> [x, y, z]
--      matchList [x, y, z, t] (PatternList: (_)) = [x, y, z, t]
--
matchList :: ExpressionMap -> [Int] -> Pattern List -> Maybe [Int]
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

-- | Match an expression with a pattern, return the map between capture hole to the actual node
-- e.g: match (Expression: (a(3243) + b(32521)) (PatternNormal:(x(1) + y(2)) --> ({1 -> 3243, 2 -> 32521}, {})
--      match (Expression sum(a(3243), b(32521), c(21321)) (PatternNormal:(sum(each(1))) --> ({}, {1 -> [3243, 32521, 21321]})
type Match = (Map Capture Int, Map ListCapture [Int])

match :: (ExpressionMap, Int) -> Pattern Normal -> Maybe Match
match (mp, n) wh =
    let unionBoth (x1, y1) (x2, y2) = (x1 `union` x2, y1 `union` y2)
        catMatch = foldl unionBoth (Map.empty, Map.empty)
        recursiveAndCombine :: [Arg] -> [Pattern Normal] -> Maybe Match
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
