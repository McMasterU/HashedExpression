{-|
Module      : HashedExpression.Solvers.HIGHS
Copyright   :  (c) Curtis D'Alves 2024
License     :  BSD (see the LICENSE file)
Maintainer  :  curtis.dalves@gmail.com
Stability   :  experimental
Portability :  portable

Description:
  Support for solving HashedExpression using the HiGHS solver. See
  https://highs.dev/
  NOTE HiGHS does its own evaluation of objective and derivative functions, so
  HashedExpression is only used as a front end for constructing the optimization model.
  Rather than using an API, we opt to generate an MPS file from an @OptimizationProblem@
  and call the highs executable, then read the result using IO. This relies on the highs
  executable being installed and accessible (i.e. in the callers PATH)
-}

module HashedExpression.Solvers.HIGHS where

import HashedExpression
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import HashedExpression.Codegen.MPS

import qualified Data.List as List

parseHiGHsOutput output = let
    outLines = lines output
    columns = takeWhile (/="Rows") $ dropWhile (/="Columns") outLines
    rawColumns = tail $ tail columns -- removes header lines
    parseLine line = let ws = words line
                     in (ws List.!! 6, read (ws List.!! 4) :: Double)
  in map parseLine rawColumns
