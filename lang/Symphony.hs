module Symphony where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import ParHashedLang
import Text.Regex.Posix

import AbsHashedLang
import ErrM
import qualified HashedExpression as HE
import qualified HashedSolver as HS
import qualified HashedUtils as HU
import LayoutHashedLang
import LexHashedLang

myLLexer :: String -> [Token]
myLLexer = resolveLayout True . myLexer

data CompileError
    = SyntaxError Int Int
    | GeneralError String

parse :: String -> Either CompileError Problem
parse fileContent =
    case pProblem . myLLexer $ fileContent of
        Ok problem -> Right problem
        Bad errStr ->
            case (errStr =~ "line ([0-9]+), column ([0-9]+)" :: ( String
                                                                , String
                                                                , String
                                                                , [String])) of
                (_, _, _, [rs, cs]) ->
                    let r = max (read rs) (length r2c) -- because layout parsing add dummy ';'
                        c = max (read cs) (getNumColumn r) -- because layout parsing add dummy ';'
                     in Left $ SyntaxError r c
                _ -> Left $ SyntaxError 1 1
  where
    r2c = Map.fromList . zip [1 ..] . map length . lines $ fileContent
    getNumColumn r = fromMaybe 0 $ Map.lookup r r2c

-- | (name, shape, initialize value)
--
type VariableBlockResult = [(String, HE.Shape, Maybe HU.Val)]

checkVariableBlock :: [VariableDecl] -> Either CompileError VariableBlockResult
checkVariableBlock variableDecls = undefined

-- | (name, shape, value)
--
type ConstantBlockResult = [(String, HE.Shape, HU.Val)]

checkConstantBlock :: [ConstantDecl] -> Either CompileError ConstantBlockResult
checkConstantBlock constantDecls = undefined

-- | TODO: Check variables block, check constant block, check operation (shape and element type), ...
--
checkSemantic :: Problem -> Either CompileError HS.Problem
checkSemantic problem = undefined
