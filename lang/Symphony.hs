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
    deriving (Show)

type Result a = Either CompileError a

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
                    let r = min (read rs) (length r2c) -- because layout parsing add dummy ';'
                        c = min (read cs) (getNumColumn r) -- because layout parsing add dummy ';'
                     in Left $ SyntaxError r c
                _ -> Left $ SyntaxError 1 1
  where
    r2c = Map.fromList . zip [1 ..] . map length . lines $ fileContent
    getNumColumn r = fromMaybe 0 $ Map.lookup r r2c

-- | TODO:
-- 1. Check if there is variables block
-- 2. Check if variable block is valid (no name clash)
-- 3. If there is a constant, check if it is valid (no name clash)
-- 4. Check if all expressions (in constraints and objective) is valid (should be scalar, all operation should be valid),
--        if yes construct Expression.
-- 5. Gen code
-- check constant block, check operation (shape and element type), ...
--
checkSemantic :: Problem -> Result HS.Problem
checkSemantic problem = do
    varDecls <- checkVariableBlockExist problem -- 1
    vars <- checkVariableBlock varDecls -- 2
    undefined

checkVariableBlockExist :: Problem -> Result [VariableDecl]
checkVariableBlockExist (Problem blocks) =
    case filter isVariableBlock blocks of
        [] -> Left $ GeneralError "No variale block"
        [BlockVariable declss] -> Right $ concat declss
        _ -> Left $ GeneralError "There are more than 1 variables block"
  where
    isVariableBlock (BlockVariable declss) = True
    isVariableBlock _ = False

-- | (name, shape, initialize value)
--
type VarInfo = (String, HE.Shape, Maybe HU.Val)

checkVariableBlock :: [VariableDecl] -> Result [VarInfo]
checkVariableBlock variableDecls = undefined

-- | (name, shape, value)
--
type ConstInfo = (String, HE.Shape, HU.Val)

checkConstantBlock :: [ConstantDecl] -> Result [ConstInfo]
checkConstantBlock constantDecls = undefined
