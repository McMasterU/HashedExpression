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
import HashedExpression.Problem
import HashedExpression.Value ( ValMap )

-- | Each element is a line of code
type Code = [Text]

-- | The type class for code generating
class Codegen configs where
  generateProblemCode :: configs -> Problem -> ValMap -> Either String (FilePath -> IO ())

-- | Indent `n` space each line of code
indent ::
  -- | The number of spaces by which to indent the code
  Int ->
  -- | The code to indent
  Code ->
  -- | The indented code
  Code
indent n = map (T.pack (replicate n ' ') <>)
