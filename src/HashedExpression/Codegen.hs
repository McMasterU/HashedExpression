-- |
-- Module      :  HashedExpression.Codegen
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module provides the interface for performing c code generation. Use this is conjunction with a backend
-- (like 'HashedExpression.Codegen.CSimple'), which will provide a corresponding instance for the 'Codegen' class
module HashedExpression.Codegen where

import Data.Text (Text)
import qualified Data.Text as T
import HashedExpression.Internal.Expression
import HashedExpression.Problem
import HashedExpression.Value

-- | Each element is a line of code
type Code = [Text]

-- | The result of a code generation
data GenResult
  = -- | Unable to generate the code along with a reason why
    Invalid String
  | -- | Success, write all the necessary files in a given file path
    Success (String -> IO ())

-- | The initial state of a code generation
data CodegenInit
  = CodegenInit
      { -- | The ExpressionMap containing the expression to generate
        codegenExMap :: ExpressionMap,
        -- FIXME: not sure what this is
        codegenConsecutiveIDs :: [Int]
        -- more common options here
      }

-- | The type class for code generating
class Codegen configs where
  generateProblemCode :: configs -> Problem -> ValMaps -> GenResult

-- | Indent `n` space each line of code
indent ::
  -- | The number of spaces by which to indent the code
  Int ->
  -- | The code to indent
  Code ->
  -- | The indented code
  Code
indent n = map (T.pack (replicate n ' ') <>)
