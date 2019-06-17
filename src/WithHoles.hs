{-
(c) 2014 Christopher Kumar Anand

Helper functions/instances to make pattern gaurds involving Expressions easier to read.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module WithHoles where

import qualified Data.IntMap as I
import qualified Data.List as L
import Data.Maybe
import HashedExpression

type Capture = Int

data WithHoles
    = WHHole Capture
    | WHConst Double
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

match :: Expression e dt -> WithHoles -> Maybe [(Capture, Int)]
match e@(Expression n mp) wh =
    let recursiveAndCombine :: [Arg] -> [WithHoles] -> Maybe [(Capture, Int)]
        recursiveAndCombine args whs
            | length args == length whs
            , let subMatches = zipWith match (map (flip Expression mp) args) whs
            , all isJust subMatches = Just . concat . catMaybes $ subMatches
            | otherwise = Nothing
     in case (expressionNode e, wh) of
            (_, WHHole capture) -> Just [(capture, n)]
            (Const c, WHConst whc)
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
