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
import Data.Complex
import Data.List (intercalate)
import Data.List.Extra (firstJust)
import Data.Map (Map)
import Data.Tuple.HT (fst3)
import ErrM
import qualified HashedExpression as HE
import HashedExpression (Node(..))
import HashedInner
import qualified HashedNode as HN
import qualified HashedOperation as HO
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

-- | (name, shape, initialize value)
--
type Vars = Map String (HE.Shape, Maybe HU.Val)

-- | (name, shape, value)
--
type Consts = Map String (HE.Shape, HU.Val)

-- | 
--
type Context = (Vars, Consts)

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
    let context = (vars, consts)
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

getMinizeBlock :: Problem -> Result Exp
getMinizeBlock (Problem blocks) =
    case filter isMinimizeBlock blocks of
        [] -> throwError $ GeneralError "No minimize block"
        [BlockMinimize exp] -> return exp
        _ -> throwError $ GeneralError "There are more than 1 let block"
  where
    isMinimizeBlock (BlockMinimize declss) = True
    isMinimizeBlock _ = False

checkVariableBlock :: [VariableDecl] -> Result Vars
checkVariableBlock = foldl f (return Map.empty)
  where
    f :: Result Vars -> VariableDecl -> Result Vars
    f acc decl = do
        accRes <- acc
        let checkName name pos =
                when (name `Map.member` accRes) $
                throwError $
                ErrorWithPosition ("Duplicate declaration of " ++ name) pos
        case decl of
            VariableNoInit (PIdent (pos, name)) shape -> do
                checkName name pos
                return $ Map.insert name (toHEShape shape, Nothing) accRes
            VariableWithInit (PIdent (pos, name)) shape val -> do
                checkName name pos
                checkVal (toHEShape shape) val
                return $
                    Map.insert name (toHEShape shape, Just (toHEVal val)) accRes

checkConstantBlock :: Vars -> [ConstantDecl] -> Result Consts
checkConstantBlock vars = foldl f (return Map.empty)
  where
    f :: Result Consts -> ConstantDecl -> Result Consts
    f acc decl = do
        accRes <- acc
        let (ConstantDecl (PIdent (pos, name)) shape val) = decl
        when (name `Map.member` accRes) $
            throwError $
            ErrorWithPosition ("Duplicate declaration of " ++ name) pos
        when (name `Map.member` vars) $
            throwError $
            ErrorWithPosition (name ++ " already defined as variables") pos
        checkVal (toHEShape shape) val
        return $ Map.insert name (toHEShape shape, toHEVal val) accRes

checkConstraintBlock ::
       Context -> [ConstraintDecl] -> Result [HS.ConstraintStatement]
checkConstraintBlock context cDcls = undefined

pInteger2Int :: PInteger -> Int
pInteger2Int (PInteger (_, val)) = read val

-- | Helpers
--
toHEShape :: Shape -> HE.Shape
toHEShape s =
    case s of
        ShapeScalar -> []
        Shape1D (Dim size1) -> [pInteger2Int size1]
        Shape2D (Dim size1) (Dim size2) ->
            [pInteger2Int size1, pInteger2Int size2]
        Shape3D (Dim size1) (Dim size2) (Dim size3) ->
            [pInteger2Int size1, pInteger2Int size2, pInteger2Int size3]

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

checkIdent :: Context -> ((Int, Int), String) -> Result (HE.Shape, Maybe HU.Val)
checkIdent (vars, consts) (pos, name)
    | Just (shape, maybeVal) <- Map.lookup name vars = return (shape, maybeVal)
    | Just (shape, val) <- Map.lookup name consts = return (shape, Just val)
    | otherwise = throwError $ ErrorWithPosition (name ++ " is undefined") pos

-- | TODO: O(n^2) complexity, improve later
--
checkExp :: Context -> Maybe HE.Shape -> Exp -> Result (HE.ExpressionMap, Int)
checkExp context inferredShape exp =
    case exp of
        ENumDouble (PDouble (pos, valStr))
            | Just shape <- inferredShape ->
                return $ HU.aConst shape (read valStr)
            | otherwise ->
                throwError $
                ErrorWithPosition ("Ambiguous shape of literal " ++ valStr) pos
        ENumInteger (PInteger (pos, valStr))
            | Just shape <- inferredShape ->
                return $ HU.aConst shape (read valStr)
            | otherwise ->
                throwError $
                ErrorWithPosition ("Ambiguous shape of literal " ++ valStr) pos
        EIdent (PIdent (idPos, name)) -> do
            (shape, _) <- checkIdent context (idPos, name)
            return (HU.varWithShape shape name)
        EPlus exp1 (TokenPlus (opPos, _)) exp2 -> do
            let sp =
                    inferredShape @> inferShape context exp1 @>
                    inferShape context exp2
            operand1 <- checkExp context sp exp1
            operand2 <- checkExp context sp exp2
            checkSameShape
                operand1
                operand2
                "Shape mismatched: trying to add 2 vectors with different shape"
                opPos
            checkSameNumType
                operand1
                operand2
                "Numtype mismatched: trying to add 2 vectors with different numtype"
                opPos
            return $ sumMany [operand1, operand2]
        ERealImag exp1 (TokenReIm (opPos, _)) exp2 -> do
            let sp =
                    inferredShape @> inferShape context exp1 @>
                    inferShape context exp2
            operand1 <- checkExp context sp exp1
            operand2 <- checkExp context sp exp2
            checkSameShape
                operand1
                operand2
                "Shape mismatched: trying to form complex from 2 vectors with different shape"
                opPos
            when (getNT operand1 /= HE.R) $
                throwError $
                ErrorWithPosition "Numtype of operand 1 is not real" opPos
            when (getNT operand2 /= HE.R) $
                throwError $
                ErrorWithPosition "Numtype of operand 2 is not real" opPos
            return $ apply (binary RealImag) [operand1, operand2]
        ESubtract exp1 (TokenSub (opPos, _)) exp2 -> do
            let sp =
                    inferredShape @> inferShape context exp1 @>
                    inferShape context exp2
            operand1 <- checkExp context sp exp1
            operand2 <- checkExp context sp exp2
            checkSameShape
                operand1
                operand2
                "Shape mismatched: trying to subtract 2 vectors with different shape"
                opPos
            checkSameNumType
                operand1
                operand2
                "Numtype mismatched: trying to subtract 2 vectors with different numtype"
                opPos
            return $
                sumMany
                    [operand1, apply (unaryET Neg ElementDefault) [operand2]]
        EMul exp1 (TokenMul (opPos, _)) exp2 -> do
            let sp =
                    inferredShape @> inferShape context exp1 @>
                    inferShape context exp2
            operand1 <- checkExp context sp exp1
            operand2 <- checkExp context sp exp2
            checkSameShape
                operand1
                operand2
                "Shape mismatched: trying to multiply 2 vectors with different shape"
                opPos
            checkSameNumType
                operand1
                operand2
                "Numtype mismatched: trying to multiply 2 vectors with different numtype"
                opPos
            return $ mulMany [operand1, operand2]
        EDiv exp1 (TokenDiv (opPos, _)) exp2 -> undefined
        EScale exp1 (TokenScale (opPos, _)) exp2 -> undefined
        EDot exp1 (TokenDot (opPos, _)) exp2 -> undefined
        EPower exp1 (TokenPower (opPos, _)) val -> undefined
        ERotate (TokenRotate (opPos, _)) rotateAmount exp -> undefined
        ENegate (TokenSub (opPos, _)) exp -> undefined
        EPiecewise (TokenCase (opPos, _)) exp cases -> undefined
        EFun (PIdent (opPos, _)) exp -> undefined

inferShape :: Context -> Exp -> Maybe HE.Shape
inferShape context@(vars, consts) exp =
    case exp of
        ENumDouble _ -> Nothing
        ENumInteger _ -> Nothing
        EIdent (PIdent (_, name))
            | Just (shape, maybeVal) <- Map.lookup name vars -> Just shape
            | Just (shape, val) <- Map.lookup name consts -> Just shape
        EPlus exp1 _ exp2 -> anyJust . map (inferShape context) $ [exp1, exp2]
        ERealImag exp1 _ exp2 ->
            anyJust . map (inferShape context) $ [exp1, exp2]
        ESubtract exp1 _ exp2 ->
            anyJust . map (inferShape context) $ [exp1, exp2]
        EMul exp1 _ exp2 -> anyJust . map (inferShape context) $ [exp1, exp2]
        EDiv exp1 _ exp2 -> anyJust . map (inferShape context) $ [exp1, exp2]
        EScale exp1 _ exp2 -> anyJust . map (inferShape context) $ [exp1, exp2]
        EDot exp1 _ exp2 -> Just []
        EPower exp1 _ val -> anyJust . map (inferShape context) $ [exp1]
        ERotate (TokenRotate _) rotateAmount exp ->
            anyJust . map (inferShape context) $ [exp]
        ENegate (TokenSub _) exp -> anyJust . map (inferShape context) $ [exp]
        EPiecewise (TokenCase _) exp cases ->
            anyJust . map (inferShape context) $ [exp]
        EFun (PIdent _) exp -> anyJust . map (inferShape context) $ [exp] -- TODO: depends on PIdent

checkSameShape ::
       (HE.ExpressionMap, Int)
    -> (HE.ExpressionMap, Int)
    -> String
    -> (Int, Int)
    -> Result ()
checkSameShape operand1 operand2 errStr pos = do
    let shape1 = getShape operand1
        shape2 = getShape operand1
    unless (shape1 == shape2) $
        throwError $
        ErrorWithPosition
            (errStr ++
             ". The shape of LHS is " ++
             toReadable shape1 ++
             ", but the shape of RHS is " ++ toReadable shape2)
            pos

checkSameNumType ::
       (HE.ExpressionMap, Int)
    -> (HE.ExpressionMap, Int)
    -> String
    -> (Int, Int)
    -> Result ()
checkSameNumType operand1 operand2 errStr pos = do
    let nt1 = getNT operand1
        nt2 = getNT operand1
    unless (nt1 == nt2) $
        throwError $
        ErrorWithPosition
            (errStr ++
             ". The numtype of operand 1 is " ++
             toReadableNT nt1 ++
             ", but the numtype of operand 2 is " ++ toReadableNT nt2)
            pos

-- | Utils
--
anyJust :: [Maybe a] -> Maybe a
anyJust = firstJust id

getShape :: (HE.ExpressionMap, Int) -> HE.Shape
getShape (mp, n) = HN.retrieveShape n mp

getNT :: (HE.ExpressionMap, Int) -> HE.ET
getNT (mp, n) = HN.retrieveElementType n mp

toReadable :: HE.Shape -> String
toReadable [] = "scalar"
toReadable xs = intercalate "x" . map show $ xs

toReadableNT :: HE.ET -> String
toReadableNT HE.R = "real"
toReadableNT HE.C = "complex"

infixl 8 @>

(@>) :: Maybe a -> Maybe a -> Maybe a
x @> y =
    case x of
        Just _ -> x
        Nothing -> y
