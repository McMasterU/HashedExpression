{-# LANGUAGE FlexibleContexts #-}

module Symphony where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import ParHashedLang
import Text.Regex.Posix

import AbsHashedLang
import Control.Monad (when)
import Control.Monad.Except
import Data.Tuple.HT (fst3)
import ErrM
import qualified HashedExpression as HE
import qualified HashedSolver as HS
import qualified HashedUtils as HU
import LayoutHashedLang
import LexHashedLang

myLLexer :: String -> [Token]
myLLexer = resolveLayout True . myLexer

data CompileError
    = SyntaxError (Int, Int)
    | GeneralError String
    | ErrorWithPosition String (Int, Int)
    deriving (Show)

type Result a = ExceptT CompileError IO a

parse :: String -> Result Problem
parse fileContent =
    case pProblem . myLLexer $ fileContent of
        Ok problem -> return problem
        Bad errStr ->
            case (errStr =~ "line ([0-9]+), column ([0-9]+)" :: ( String
                                                                , String
                                                                , String
                                                                , [String])) of
                (_, _, _, [rs, cs]) ->
                    let r = min (read rs) (length r2c) -- because layout parsing add dummy ';'
                        c = min (read cs) (getNumColumn r) -- because layout parsing add dummy ';'
                     in throwError $ SyntaxError (r, c)
                _ -> throwError $ SyntaxError (1, 1)
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
    varDecls <- getVarDecls problem -- 1
    vars <- checkVariableBlock varDecls -- 2
    constDecls <- getConstDecls problem
    consts <- checkConstantBlock vars constDecls
    constraintDecls <- getConstraintDecls problem
    throwError $ GeneralError "TODO: haven't completed yet"
  where
    isVariableBlock (BlockVariable declss) = True
    isVariableBlock _ = False

getVarDecls :: Problem -> Result [VariableDecl]
getVarDecls (Problem blocks) =
    case filter isVariableBlock blocks of
        [] -> throwError $ GeneralError "No variale block"
        [BlockVariable declss] -> return $ concat declss
        _ -> throwError $ GeneralError "There are more than 1 variables block"
  where
    isVariableBlock (BlockVariable declss) = True
    isVariableBlock _ = False

getConstDecls :: Problem -> Result [ConstantDecl]
getConstDecls (Problem blocks) =
    case filter isConstantBlock blocks of
        [] -> return []
        [BlockConstant declss] -> return $ concat declss
        _ -> throwError $ GeneralError "There are more than 1 constant block"
  where
    isConstantBlock (BlockConstant declss) = True
    isConstantBlock _ = False

getConstraintDecls :: Problem -> Result [ConstraintDecl]
getConstraintDecls (Problem blocks) =
    case filter isConstraintBlock blocks of
        [] -> return []
        [BlockConstraint declss] -> return $ concat declss
        _ -> throwError $ GeneralError "There are more than 1 constraint block"
  where
    isConstraintBlock (BlockConstraint declss) = True
    isConstraintBlock _ = False

getLetDecls :: Problem -> Result [LetDecl]
getLetDecls (Problem blocks) =
    case filter isLetBlock blocks of
        [] -> return []
        [BlockLet declss] -> return $ concat declss
        _ -> throwError $ GeneralError "There are more than 1 let block"
  where
    isLetBlock (BlockLet declss) = True
    isLetBlock _ = False

-- | (name, shape, initialize value)
--
type VarInfo = (String, HE.Shape, Maybe HU.Val)

checkVariableBlock :: [VariableDecl] -> Result [VarInfo]
checkVariableBlock = foldl f (return [])
  where
    f :: Result [VarInfo] -> VariableDecl -> Result [VarInfo]
    f acc decl = do
        accRes <- acc
        let checkName name pos =
                when (name `elem` map fst3 accRes) $
                throwError $
                ErrorWithPosition ("Duplicate declaration of " ++ name) pos
        case decl of
            VariableNoInit (PIdent (pos, name)) shape -> do
                checkName name pos
                return $ (name, toHEShape shape, Nothing) : accRes
            VariableWithInit (PIdent (pos, name)) shape val -> do
                checkName name pos
                checkVal (toHEShape shape) val
                return $ (name, toHEShape shape, Just (toHEVal val)) : accRes

-- | (name, shape, value)
--
type ConstInfo = (String, HE.Shape, HU.Val)

checkConstantBlock :: [VarInfo] -> [ConstantDecl] -> Result [ConstInfo]
checkConstantBlock varInfos = foldl f (return [])
  where
    vars = Set.fromList $ map fst3 varInfos
    f :: Result [ConstInfo] -> ConstantDecl -> Result [ConstInfo]
    f acc decl = do
        accRes <- acc
        let (ConstantDecl (PIdent (pos, name)) shape val) = decl
        when (name `elem` map fst3 accRes) $
            throwError $
            ErrorWithPosition ("Duplicate declaration of " ++ name) pos
        when (name `Set.member` vars) $
            throwError $
            ErrorWithPosition (name ++ " already defined in variables") pos
        checkVal (toHEShape shape) val
        return $ (name, toHEShape shape, toHEVal val) : accRes


checkConstraintBlock ::
       [VarInfo] -> [ConstInfo] -> [ConstraintDecl] -> Result [HS.ConstraintStatement]
checkConstraintBlock varInfos constInfo cDcls = undefined

-- | Helpers
--
toHEShape :: Shape -> HE.Shape
toHEShape s =
    case s of
        ShapeScalar -> []
        Shape1D (Dim size1) -> [fromIntegral size1]
        Shape2D (Dim size1) (Dim size2) ->
            [fromIntegral size1, fromIntegral size2]
        Shape3D (Dim size1) (Dim size2) (Dim size3) ->
            [fromIntegral size1, fromIntegral size2, fromIntegral size3]

-- |  TODO: To corresponding val
--
toHEVal :: Val -> HU.Val
toHEVal v =
    case v of
        ValFile _ -> HU.VNum 3
        ValDataset _ _ -> HU.VNum 3
        ValPattern _ -> HU.VNum 3
        ValRandom -> HU.VNum 3
        ValLiteral _ -> HU.VNum 3

-- |  TODO: Check if val is valid w.r.t shape
--
checkVal :: HE.Shape -> Val -> Result ()
checkVal shape val = return ()
