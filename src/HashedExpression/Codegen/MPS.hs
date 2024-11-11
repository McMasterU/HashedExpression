{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  HashedExpression.Codegen.MPS
-- Copyright   :  (c) OCA 2024
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  curtis.dalves@gmail.com
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module provides contains functionality for transforming a HashedExpression @OptimizationProblem@
-- into MPS format (see https://www.ibm.com/docs/en/icos/22.1.1?topic=standard-records-in-mps-format)
-- Many solvers support loading MPS files, although this will not take advantage of any optimizations
-- HashedExpression provides for computation of objetive functions, gradients etc

module HashedExpression.Codegen.MPS
       (toMPS)
       where

import HashedExpression
import HashedExpression.Codegen
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import HashedExpression.Internal
import HashedExpression.Internal.Pattern
import HashedExpression.Differentiation.Reverse (partialDerivativesMap)
import HashedExpression.Problem
import HashedExpression.Interp

import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (isJust,fromJust)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

-- | Generate MPS code form an @OptimizationProblem@
-- MPS is a standardized file format for encoding linear optimization problems.
-- Most solvers (including cplex, highs, etc) support MPS.
-- For documentation on the format standards see
-- <https://www.ibm.com/docs/en/icos/22.1.1?topic=standard-records-in-mps-format>
toMPS :: OptimizationProblem -> Code
toMPS (OptimizationProblem objective constraints values) =
  let
    prob@(Problem variables objectiveId expressionMap boxConstraints generalConstraints)
      = case constructProblem objective constraints of
          Left err -> error $ "error constructing problem: " ++ err
          Right prob -> prob
    valMap = mkValMap values

    -- ROWS Section
    -- ---------------------------------------------------------------------------
    rowsSection =
      ["ROWS"
      ,"  N  obj"]
      ++ map (\(constrName,_) -> printSpaced ("L",constrName,"")) namedConstraints

    namedConstraints = zip [Text.pack ("c" ++ show i) | i <- [0..]] generalConstraints

    -- COLUMNS Section
    -- ---------------------------------------------------------------------------
    columnsSection = "COLUMNS" :
                     map printSpaced colVals
    colVals = concatMap -- NOTE variables (i.e columns) need to be grouped together
                  (\(idx,var) -> let
                                    constrVals = map (\(cName,gConstr@(GeneralConstraint vID pIDs lb ub)) ->
                                                  let
                                                    scalar = fromJust
                                                            $ exprIsConstant
                                                            -- NOTE order of variables list corresponds to pIDs
                                                            $ simplify (expressionMap,pIDs List.!! idx)
                                                  in (Text.pack $ varName var,cName,Text.pack $ show scalar))
                                              namedConstraints
                                    objVal = let
                                                scalar = fromJust $ exprIsConstant
                                                                  $ simplify (expressionMap,partialDerivativeId var)
                                            in (Text.pack $ varName var,"obj",Text.pack $ show scalar)
                                  in objVal : constrVals
                                 ) $ zip [0..] variables

    -- RHS Section
    -- ---------------------------------------------------------------------------
    rhsSection = "RHS" :
                 map printSpaced (objRHS : constraintsRHS)
    -- NOTE assumes linear objective with only VScalar variables
    zeroValMap = Map.fromList $ map (\var -> (varName var,VScalar 0.0)) variables
    objOffset = case eval zeroValMap (expressionMap,objectiveId) of
                  VR val -> val
                  interpVal ->
                    error $ "RHS evaluation of objective function returned non-scalar: "
                            ++ show interpVal
    objRHS = ("rhs","obj",Text.pack $ show $ -objOffset)
    -- constraint RHS are all upper bounds, lower bounds are defined using ranges
    constraintsRHS = map (\(cName,GeneralConstraint vID pIDs lb ub) ->
                      ("rhs",cName,Text.pack $ show ub)) namedConstraints

    -- RANGES Section
    -- ---------------------------------------------------------------------------
    rangesSection = "RANGES" :
                    map printSpaced constraintsRanges
    -- lower bounds are defined as a range, where lowerbound = contraintsRHS - range
    constraintsRanges = map (\(cName,GeneralConstraint vID pIDs lb ub) ->
                      ("rhs",cName,Text.pack $ show $ ub-lb)) namedConstraints

    -- BOUNDS Section
    -- ---------------------------------------------------------------------------
    boundsSection = "BOUNDS" :
                    map printSpaced varBounds
    -- box constraints are defined in the bound section
    lookupBoundVal boundID = case Map.lookup boundID valMap of
                                Just (VScalar d) -> d
                                Just s -> error $ "bound is not VScalar " ++ show s
                                Nothing -> error $ "bound missing: " ++ show boundID
    varBounds = map (\bc -> case bc of
                              BoxUpper vName boundID ->
                                ("UP BOUND",Text.pack vName,Text.pack $ show $ lookupBoundVal boundID)
                              BoxLower vName boundID ->
                                ("LO BOUND",Text.pack vName,Text.pack $ show $ lookupBoundVal boundID)
                             ) boxConstraints

    -- MPS Files
    -- ---------------------------------------------------------------------------
    mpsFile =  ["NAME        HASHEDEXPR"] -- TODO put date time stamp in name?
               ++ rowsSection
               ++ columnsSection
               ++ rhsSection
               ++ rangesSection
               ++ boundsSection
               ++ ["ENDATA"]
  in if exprIsLinear (expressionMap,objectiveId)
       then mpsFile
       else error "toMPS given non-linear optimization problem"

indent8 :: Text -> Text
indent8 s = "        " <> s

printSpaced :: (Text, Text, Text) -> Text
printSpaced (s0,s1,s2) = indent8 s0 <> indent8 s1 <> indent8 s2

exprIsLinear :: IsScalarReal e => e -> Bool
exprIsLinear expr =
  let
    (exprMap,derivativeMap) = partialDerivativesMap expr
    derivativeNodeIDs = Map.elems derivativeMap
     -- expression is linear if forall p is a partial derivative, p is constant
  in all (\nID ->
                  isJust $ exprIsConstant $ simplify (exprMap,nID)) derivativeNodeIDs

exprIsConstant :: IsExpression e => e -> Maybe Double
exprIsConstant expr =
  let
    (exprMap,nID) = asRawExpr expr
  in case IntMap.lookup (unNodeID nID) exprMap of
       Just ([],R,Const d) -> Just d
       -- To check if the expression was a variable and return its label you could use
       -- Just (_,_,Var s) -> Just s
       -- To check if the top of the expression is a multiplication
       -- Just (_,_,Mul args) -> Just args
       _ -> Nothing
