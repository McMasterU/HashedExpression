module Symphony where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import ParHashedLang
import Text.Regex.Posix

import AbsHashedLang
import ErrM
import qualified HashedSolver as HS
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

checkSemantic :: Problem -> Either CompileError HS.Problem
checkSemantic problem = undefined
