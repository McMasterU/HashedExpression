{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Symphony where

import qualified Data.Array as Array
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
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
import qualified HashedExpression.Expression as HE
import HashedExpression.Expression(ExpressionMap, Node(..))
import HashedExpression.Inner
import qualified HashedNode as HN
import qualified HashedOperation as HO
import HashedPrettify
import qualified HashedSolver as HS
import qualified HashedExpression.Utils as HU
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

data Context =
    Context
        { declarations :: Map String (ExpressionMap, Int)
        , vars :: Vars
        , consts :: Consts
        }

-- | TODO:
-- 1. Check if there is variables block
-- 2. Check if variable block is valid (no name clash)
-- 3. If there is a constant, check if it is valid (no name clash)
-- 4. Check if all expressions (in constraints and objective) is valid (should be scalar, all operation should be valid),
--        if yes construct Expression.
-- 5. Gen code
-- check constant block, check operation (shape and element type), ...
--
checkSemanticAndGenCode :: Problem -> Result (String -> IO ())
checkSemanticAndGenCode problem = do
    varDecls <- getVarDecls problem -- 1
    vars <- foldM processVariableDecl Map.empty varDecls -- 2
    constDecls <- getConstDecls problem
    consts <- foldM (processConstantDecl vars) Map.empty constDecls
    let initContext =
            Context
                { declarations =
                      Map.mapWithKey toExp vars `Map.union`
                      Map.mapWithKey toExp consts
                , vars = vars
                , consts = consts
                }
    letDecls <- getLetDecls problem
    finalContext <- foldM processLetDecl initContext letDecls
    -- Complete processing context (variables, constants, declared values)
    -- Constraint
    constraintDecls <- getConstraintDecls problem
    css <- foldM (processConstraintDecl finalContext) [] constraintDecls
    -- Objective
    parseObjectiveExp <- getMinimizeBlock problem
    objectiveExp <- constructExp finalContext (Just []) parseObjectiveExp
    let shape = getShape objectiveExp
        nt = getNT objectiveExp
    when (shape /= [] || nt /= HE.R) $
        throwError $
        ErrorWithPosition
            ("Objective should be a real scalar, here it is (" ++
             toReadable shape ++ ", " ++ show nt ++ ")")
            (getBeginningPosition parseObjectiveExp)
    liftIO $ putStrLn "Syntax is correct"
    liftIO $
        putStrLn
            "Now compute gradient, simplify, eliminate common subexpressions and generate code..."
    let problemGen =
            HS.constructProblem
                (wrap objectiveExp)
                (Map.keys vars)
                (HS.Constraint css)
    case problemGen of
        HS.ProblemValid heProblem -> do
            let valMap =
                    Map.mapMaybeWithKey varVal vars `Map.union`
                    Map.mapMaybeWithKey constVal consts
            case HS.generateProblemCode valMap heProblem of
                HS.Invalid reason -> throwError $ GeneralError reason
                HS.Success res -> return res
        HS.ProblemInvalid reason -> throwError $ GeneralError reason
  where
    toExp name (shape, _) = HU.varWithShape shape name
    varVal name (_, Just val) = Just val
    varVal _ _ = Nothing
    constVal name (_, val) = Just val

-- | Get decls from blocks
--
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

getMinimizeBlock :: Problem -> Result Exp
getMinimizeBlock (Problem blocks) =
    case filter isMinimizeBlock blocks of
        [] -> throwError $ GeneralError "No minimize block"
        [BlockMinimize exp] -> return exp
        _ -> throwError $ GeneralError "There are more than 1 minimize block"
  where
    isMinimizeBlock (BlockMinimize declss) = True
    isMinimizeBlock _ = False

-- | Process declaration
--
processVariableDecl :: Vars -> VariableDecl -> Result Vars
processVariableDecl accRes decl = do
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
            let heShape = toHEShape shape
            heVal <- toHEVal heShape val
            return $ Map.insert name (heShape, Just heVal) accRes

processConstantDecl :: Vars -> Consts -> ConstantDecl -> Result Consts
processConstantDecl vars accRes decl = do
    let (ConstantDecl (PIdent (pos, name)) shape val) = decl
    when (name `Map.member` accRes) $
        throwError $ ErrorWithPosition ("Duplicate declaration of " ++ name) pos
    when (name `Map.member` vars) $
        throwError $
        ErrorWithPosition (name ++ " already defined as variables") pos
    let heShape = toHEShape shape
    checkVal heShape val
    heVal <- toHEVal heShape val
    return $ Map.insert name (toHEShape shape, heVal) accRes

processConstraintDecl ::
       Context
    -> [HS.ConstraintStatement]
    -> ConstraintDecl
    -> Result [HS.ConstraintStatement]
processConstraintDecl context@Context {..} acc decl = do
    (constructedExp, boundVal) <-
        case (isVariable exp, bound) of
            (Just varExp, ConstantBound (PIdent (pos, name)))
                | Just (shape, val) <- Map.lookup name consts -> do
                    when (shape /= getShape varExp) $
                        throwError $
                        ErrorWithPosition
                            "Shape mismatched: the bound doesn't have same shape as the variable"
                            pos
                    return (varExp, val)
                | otherwise ->
                    throwError $ ErrorWithPosition (name ++ " not found") pos
            (Just varExp, NumberBound num) ->
                return (varExp, HU.VNum (numToDouble num))
            _ -> do
                scalarExp <- constructExp context Nothing exp
                when (getShape scalarExp /= []) $
                    throwError $
                    ErrorWithPosition
                        ("Higher-dimension (in)equality is not supported yet, here the expression has shape " ++
                         toReadable (getShape scalarExp))
                        (getBeginningPosition exp)
                case bound of
                    ConstantBound (PIdent (pos, name))
                        | Just (shape, val) <- Map.lookup name consts -> do
                            when (shape /= getShape scalarExp) $
                                throwError $
                                ErrorWithPosition "The bound must be scalar" pos
                            return (scalarExp, val)
                        | otherwise ->
                            throwError $
                            ErrorWithPosition (name ++ " not found") pos
                    NumberBound num ->
                        return (scalarExp, HU.VNum (numToDouble num))
    let newEntry =
            case decl of
                ConstraintLower {} -> HS.Lower constructedExp boundVal
                ConstraintUpper {} -> HS.Upper constructedExp boundVal
                ConstraintEqual {} ->
                    HS.Between constructedExp (boundVal, boundVal)
    return $ acc ++ [newEntry]
  where
    (exp, bound) =
        case decl of
            ConstraintLower exp bound -> (exp, bound)
            ConstraintUpper exp bound -> (exp, bound)
            ConstraintEqual exp bound -> (exp, bound)
    isVariable exp =
        case exp of
            EIdent (PIdent (_, name))
                | Just constructedExp <- Map.lookup name declarations
                , name `Map.member` vars -> Just constructedExp
            _ -> Nothing

processLetDecl :: Context -> LetDecl -> Result Context
processLetDecl context@Context {..} (LetDecl (PIdent (pos, name)) exp) = do
    when (name `Map.member` consts) $
        throwError $
        ErrorWithPosition (name ++ " already defined as a constant") pos
    when (name `Map.member` vars) $
        throwError $
        ErrorWithPosition (name ++ " already defined as a variable") pos
    when (name `Map.member` declarations) $
        throwError $ ErrorWithPosition (name ++ " already taken") pos
    constructedExp <- constructExp context Nothing exp
    let newDeclarations = Map.insert name constructedExp declarations
    return $ context {declarations = newDeclarations}

-- | Helpers, convert between parse type and HashedExpression type
--
pInteger2Int :: PInteger -> Int
pInteger2Int (PInteger (_, val)) = read val

toHEShape :: Shape -> HE.Shape
toHEShape s =
    case s of
        ShapeScalar -> []
        Shape1D (Dim size1) -> [pInteger2Int size1]
        Shape2D (Dim size1) (Dim size2) ->
            [pInteger2Int size1, pInteger2Int size2]
        Shape3D (Dim size1) (Dim size2) (Dim size3) ->
            [pInteger2Int size1, pInteger2Int size2, pInteger2Int size3]

toInt :: TInt -> Int
toInt i =
    case i of
        IntPos (PInteger (_, strVal)) -> read strVal
        IntNeg _ (PInteger (_, strVal)) -> -(read strVal)

toDouble :: TDouble -> Double
toDouble i =
    case i of
        DoublePos (PDouble (_, strVal)) -> read strVal
        DoubleNeg _ (PDouble (_, strVal)) -> -(read strVal)

numToDouble :: Number -> Double
numToDouble number =
    case number of
        NumInt tInt -> fromIntegral $ toInt tInt
        NumDouble tDouble -> toDouble tDouble

toRotateAmount :: RotateAmount -> [Int]
toRotateAmount ra =
    case ra of
        RA1D i -> [toInt i]
        RA2D i1 i2 -> [toInt i1, toInt i2]
        RA3D i1 i2 i3 -> [toInt i1, toInt i2, toInt i3]

getBeginningPosition :: Exp -> (Int, Int)
getBeginningPosition exp =
    case exp of
        EPlus exp _ _ -> getBeginningPosition exp
        ERealImag exp _ _ -> getBeginningPosition exp
        ESubtract exp _ _ -> getBeginningPosition exp
        EMul exp _ _ -> getBeginningPosition exp
        EDiv exp _ _ -> getBeginningPosition exp
        EScale exp _ _ -> getBeginningPosition exp
        EDot exp _ _ -> getBeginningPosition exp
        EPower exp _ _ -> getBeginningPosition exp
        ERotate (TokenRotate (pos, _)) _ _ -> pos
        ENegate (TokenSub (pos, _)) _ -> pos
        ENumDouble (PDouble (pos, _)) -> pos
        ENumInteger (PInteger (pos, _)) -> pos
        EIdent (PIdent (pos, _)) -> pos
        EPiecewise (TokenCase (pos, _)) exp _ -> pos
        EUnaryFun (PUnaryFun (pos, _)) _ -> pos
        EDoubleFun (PDoubleFun (pos, _)) _ _ -> pos

toHEVal :: HE.Shape -> Val -> Result HU.Val
toHEVal shape v =
    case v of
        ValFile filePath -> return $ HU.VFile $ HU.TXT filePath
        ValDataset filePath dataset ->
            return $ HU.VFile $ HU.HDF5 filePath dataset
        ValPattern (KWDataPattern pattern) ->
            case (pattern, shape) of
                ("FIRST_ROW_1", [size1, size2]) ->
                    return .
                    HU.V2D . Array.listArray ((0, 0), (size1 - 1, size2 - 1)) $
                    replicate size2 1 ++ repeat 0
                ("LAST_ROW_1", [size1, size2]) ->
                    return .
                    HU.V2D . Array.listArray ((0, 0), (size1 - 1, size2 - 1)) $
                    replicate (size2 * (size1 - 1)) 0 ++ repeat 1
                ("FIRST_COLUMN_1", [size1, size2]) ->
                    return .
                    HU.V2D . Array.listArray ((0, 0), (size1 - 1, size2 - 1)) $
                    concat $ replicate size1 $ 1 : replicate (size2 - 1) 0
                ("LAST_COLUMN_1", [size1, size2]) ->
                    return .
                    HU.V2D . Array.listArray ((0, 0), (size1 - 1, size2 - 1)) $
                    concat $ replicate size1 $ replicate (size2 - 1) 0 ++ [1]
                ("FIRST_ROW_0", [size1, size2]) ->
                    return .
                    HU.V2D . Array.listArray ((0, 0), (size1 - 1, size2 - 1)) $
                    replicate size2 0 ++ repeat 1
                ("LAST_ROW_0", [size1, size2]) ->
                    return .
                    HU.V2D . Array.listArray ((0, 0), (size1 - 1, size2 - 1)) $
                    replicate (size2 * (size1 - 1)) 1 ++ repeat 0
                ("FIRST_COLUMN_0", [size1, size2]) ->
                    return .
                    HU.V2D . Array.listArray ((0, 0), (size1 - 1, size2 - 1)) $
                    concat $ replicate size1 $ 0 : replicate (size2 - 1) 1
                ("LAST_COLUMN_0", [size1, size2]) ->
                    return .
                    HU.V2D . Array.listArray ((0, 0), (size1 - 1, size2 - 1)) $
                    concat $ replicate size1 $ replicate (size2 - 1) 1 ++ [0]
                _ ->
                    throwError $
                    GeneralError $
                    "Pattern " ++
                    pattern ++
                    " is incompatible with the shape or not supported yet"
        ValRandom -> return $ HU.VNum 3
        ValLiteral num -> return $ HU.VNum (numToDouble num)

-- |  TODO: Check if val is valid w.r.t shape
--
checkVal :: HE.Shape -> Val -> Result ()
checkVal shape val = return ()

retrieveExpFromIdent ::
       Context -> ((Int, Int), String) -> Result (ExpressionMap, Int)
retrieveExpFromIdent context@Context {..} (pos, name)
    | Just exp <- Map.lookup name declarations = return exp
    | otherwise = throwError $ ErrorWithPosition (name ++ " is undefined") pos

-- | TODO: O(n^2) complexity, improve later
--
constructExp :: Context -> Maybe HE.Shape -> Exp -> Result (ExpressionMap, Int)
constructExp context shapeInfo exp =
    let add x y = sumMany [x, y]
        multiply x y = mulMany [x, y]
        reIm x y = apply (binary RealImag) [x, y]
        scale x y = apply (binaryET Scale ElementDefault) [x, y]
        subtract x y = sumMany [x, scale (HU.aConst [] (-1)) y]
        dot x y = apply (binaryET InnerProd ElementDefault `hasShape` []) [x, y]
        app fun x = apply (unary fun) [x]
        piecewise marks condition branches =
            apply (conditionAry (Piecewise marks)) (condition : branches)
     in case exp of
            ENumDouble (PDouble (pos, valStr))
                | Just shape <- shapeInfo ->
                    return $ HU.aConst shape (read valStr)
                | otherwise ->
                    throwError $
                    ErrorWithPosition
                        ("Ambiguous shape of literal " ++ valStr)
                        pos
            ENumInteger (PInteger (pos, valStr))
                | Just shape <- shapeInfo ->
                    return $ HU.aConst shape (read valStr)
                | otherwise ->
                    throwError $
                    ErrorWithPosition
                        ("Ambiguous shape of literal " ++ valStr)
                        pos
            EIdent (PIdent (idPos, name)) ->
                retrieveExpFromIdent context (idPos, name)
            EPlus exp1 (TokenPlus (opPos, _)) exp2 -> do
                let inferredShape =
                        inferShape context exp1 @> inferShape context exp2 @>
                        shapeInfo
                operand1 <- constructExp context inferredShape exp1
                operand2 <- constructExp context inferredShape exp2
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
                return $ add operand1 operand2
            ERealImag exp1 (TokenReIm (opPos, _)) exp2 -> do
                let inferredShape =
                        inferShape context exp1 @> inferShape context exp2 @>
                        shapeInfo
                operand1 <- constructExp context inferredShape exp1
                operand2 <- constructExp context inferredShape exp2
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
                return $ reIm operand1 operand2
            ESubtract exp1 (TokenSub (opPos, _)) exp2 -> do
                let inferredShape =
                        inferShape context exp1 @> inferShape context exp2 @>
                        shapeInfo
                operand1 <- constructExp context inferredShape exp1
                operand2 <- constructExp context inferredShape exp2
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
                return $ add operand1 (scale (HU.aConst [] (-1)) operand2)
            EMul exp1 (TokenMul (opPos, _)) exp2 -> do
                let inferredShape =
                        inferShape context exp1 @> inferShape context exp2 @>
                        shapeInfo
                operand1 <- constructExp context inferredShape exp1
                operand2 <- constructExp context inferredShape exp2
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
                return $ multiply operand1 operand2
            EDiv exp1 (TokenDiv (opPos, _)) exp2 -> do
                let inferredShape =
                        inferShape context exp1 @> inferShape context exp2 @>
                        shapeInfo
                operand1 <- constructExp context inferredShape exp1
                operand2 <- constructExp context inferredShape exp2
                checkSameShape
                    operand1
                    operand2
                    "Shape mismatched: trying to divide 2 vectors with different shape"
                    opPos
                checkSameNumType
                    operand1
                    operand2
                    "Numtype mismatched: trying to divide 2 vectors with different numtype"
                    opPos
                return $ multiply operand1 (app (Power (-1)) operand2)
            EScale exp1 (TokenScale (opPos, _)) exp2 -> do
                let inferredShape = inferShape context exp1 @> shapeInfo
                operand1 <- constructExp context (Just []) exp1
                operand2 <- constructExp context inferredShape exp2
                when (getShape operand1 /= []) $
                    throwError $
                    ErrorWithPosition
                        "The first operand of scaling operator is not a scalar"
                        opPos
                when (getNT operand1 == HE.C && getNT operand2 == HE.R) $
                    throwError $
                    ErrorWithPosition
                        "Can't scale a real vector by a complex scalar"
                        opPos
                return $ scale operand1 operand2
            EDot exp1 (TokenDot (opPos, _)) exp2 -> do
                let inferredShape =
                        inferShape context exp1 @> inferShape context exp2
                operand1 <- constructExp context inferredShape exp1
                operand2 <- constructExp context inferredShape exp2
                checkSameShape
                    operand1
                    operand2
                    "Shape mismatched: trying to take inner product of 2 vectors with different shape"
                    opPos
                checkSameNumType
                    operand1
                    operand2
                    "Numtype mismatched: trying to take inner product of 2 vectors with different numtype"
                    opPos
                return $ dot operand1 operand2
            EPower exp1 (TokenPower (opPos, _)) val -> do
                operand1 <- constructExp context shapeInfo exp1
                return $ app (Power (toInt val)) operand1
            ERotate (TokenRotate (opPos, _)) raStr exp -> do
                operand <- constructExp context shapeInfo exp
                let rotateAmount = toRotateAmount raStr
                let shape = getShape operand
                when (null shape) $
                    throwError $ ErrorWithPosition "Can't rotate a scalar" opPos
                when (length shape /= length rotateAmount) $
                    throwError $
                    ErrorWithPosition
                        ("The rotate amount is for " ++
                         show (length rotateAmount) ++
                         "d vector, but the operand is " ++
                         show (length shape) ++ "d vector")
                        opPos
                return $ app (Rotate rotateAmount) operand
            ENegate (TokenSub (opPos, _)) exp -> do
                operand <- constructExp context shapeInfo exp
                return $ scale (HU.aConst [] (-1)) operand
            EPiecewise (TokenCase (opPos, _)) exp cases -> do
                condition <- constructExp context shapeInfo exp
                let isLastCase pwcase =
                        case pwcase of
                            PiecewiseFinalCase {} -> True
                            _ -> False
                    extractMark pwcase =
                        case pwcase of
                            PiecewiseCase val _ -> [numToDouble val]
                            PiecewiseFinalCase {} -> []
                    extractExp pwcase =
                        case pwcase of
                            PiecewiseCase _ exp -> exp
                            PiecewiseFinalCase exp -> exp
                    marks = concatMap extractMark cases
                    exps = map extractExp cases
                    shape = getShape condition
                unless (isLastCase (last cases)) $
                    throwError $
                    ErrorWithPosition
                        "Piecewise must end with an otherwise case"
                        opPos
                unless ((Set.toList . Set.fromList $ marks) == marks) $
                    throwError $
                    ErrorWithPosition "The bounds must be increasing" opPos
                unless (getNT condition == HE.R) $
                    throwError $
                    ErrorWithPosition "Condition is not a real vector" opPos
                caseExps <- mapM (constructExp context (Just shape)) exps
                forM_ (zip caseExps [1 ..]) $ \(e, idx) ->
                    unless (getShape e == shape) $
                    throwError $
                    ErrorWithPosition
                        (". The shape of condition is " ++
                         toReadable shape ++
                         ", but the shape of branch " ++
                         show idx ++ " is " ++ toReadable (getShape e))
                        opPos
                unless (HU.allEqual $ map getNT caseExps) $
                    throwError $
                    ErrorWithPosition
                        "vector branches of piecewise are not of same numtype"
                        opPos
                return $ piecewise marks condition caseExps
            EUnaryFun (PUnaryFun (opPos, funName)) exp -> do
                let onlyForRealVector operand =
                        unless (getNT operand == HE.R) $
                        throwError $
                        ErrorWithPosition "Operand must be a real vector" opPos
                    onlyForComplexVector operand =
                        unless (getNT operand == HE.C) $
                        throwError $
                        ErrorWithPosition
                            "Operand must be a complex vector"
                            opPos
                operand <- constructExp context shapeInfo exp
                case funName of
                    "sqrt" -> do
                        onlyForRealVector operand
                        return $ app Sqrt operand
                    "sin" -> do
                        onlyForRealVector operand
                        return $ app Sin operand
                    "cos" -> do
                        onlyForRealVector operand
                        return $ app Cos operand
                    "tan" -> do
                        onlyForRealVector operand
                        return $ app Tan operand
                    "exp" -> do
                        onlyForRealVector operand
                        return $ app Exp operand
                    "log" -> do
                        onlyForRealVector operand
                        return $ app Log operand
                    "sinh" -> do
                        onlyForRealVector operand
                        return $ app Sinh operand
                    "cosh" -> do
                        onlyForRealVector operand
                        return $ app Cosh operand
                    "tanh" -> do
                        onlyForRealVector operand
                        return $ app Tanh operand
                    "asin" -> do
                        onlyForRealVector operand
                        return $ app Asin operand
                    "acos" -> do
                        onlyForRealVector operand
                        return $ app Acos operand
                    "atan" -> do
                        onlyForRealVector operand
                        return $ app Atan operand
                    "asinh" -> do
                        onlyForRealVector operand
                        return $ app Asin operand
                    "acosh" -> do
                        onlyForRealVector operand
                        return $ app Acos operand
                    "atanh" -> do
                        onlyForRealVector operand
                        return $ app Atan operand
                    "xRe" -> do
                        onlyForComplexVector operand
                        return $ app RealPart operand
                    "xIm" -> do
                        onlyForComplexVector operand
                        return $ app ImagPart operand
                    "ft" -> do
                        let reFT = app ReFT operand
                        let imFT = app ImFT operand
                        return $ reIm reFT imFT
                    "sumElements" -> do
                        onlyForRealVector operand
                        return $ dot operand (HU.aConst (getShape operand) 1)
                    "norm2square" -> do
                        operand <- constructExp context shapeInfo exp
                        if getNT operand == HE.R
                            then return $ dot operand operand
                            else do
                                let re = app RealPart operand
                                    im = app ImagPart operand
                                return $ dot re re `add` dot im im
                    _ ->
                        throwError $
                        ErrorWithPosition
                            ("Function " ++ funName ++ " not found")
                            opPos
            EDoubleFun (PDoubleFun (funPos, name)) num exp -> do
                let onlyForRealVector operand =
                        unless (getNT operand == HE.R) $
                        throwError $
                        ErrorWithPosition "Operand must be a real vector" funPos
                    onlyForComplexVector operand =
                        unless (getNT operand == HE.C) $
                        throwError $
                        ErrorWithPosition
                            "Operand must be a complex vector"
                            funPos
                operand <- constructExp context shapeInfo exp
                let delta = numToDouble num
                let huber delta operand =
                        let one = HU.aConst (getShape operand) 1
                            const val = HU.aConst [] val
                            inner =
                                const 0.5 `scale` (operand `multiply` operand)
                            outerLeft =
                                (const (-delta) `scale` operand) `subtract`
                                (const (delta * delta / 2) `scale` one)
                            outerRight =
                                (const delta `scale` operand) `subtract`
                                (const (delta * delta / 2) `scale` one)
                         in piecewise
                                [-delta, delta]
                                operand
                                [outerLeft, inner, outerRight]
                case name of
                    "huber" -> do
                        onlyForRealVector operand
                        return $ huber delta operand
                    "normHuber" -> do
                        onlyForRealVector operand
                        return $ dot (huber delta operand) (HU.aConst (getShape operand) 1)

inferShape :: Context -> Exp -> Maybe HE.Shape
inferShape context@Context {..} exp =
    case exp of
        ENumDouble _ -> Nothing
        ENumInteger _ -> Nothing
        EIdent (PIdent (_, name))
            | Just exp <- Map.lookup name declarations -> Just $ getShape exp
            | otherwise -> Nothing
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
        EUnaryFun (PUnaryFun _) exp ->
            anyJust . map (inferShape context) $ [exp] -- TODO: depends on PIdent
        EDoubleFun (PDoubleFun _) _ exp ->
            anyJust . map (inferShape context) $ [exp] -- TODO: depends on PIdent

checkSameShape ::
       (ExpressionMap, Int)
    -> (ExpressionMap, Int)
    -> String
    -> (Int, Int)
    -> Result ()
checkSameShape operand1 operand2 errStr pos = do
    let shape1 = getShape operand1
        shape2 = getShape operand2
    unless (shape1 == shape2) $
        throwError $
        ErrorWithPosition
            (errStr ++
             ". The shape of 1st operand is " ++
             toReadable shape1 ++
             ", but the shape of 2nd operand 2 is " ++ toReadable shape2)
            pos

checkSameNumType ::
       (ExpressionMap, Int)
    -> (ExpressionMap, Int)
    -> String
    -> (Int, Int)
    -> Result ()
checkSameNumType operand1 operand2 errStr pos = do
    let nt1 = getNT operand1
        nt2 = getNT operand2
    unless (nt1 == nt2) $
        throwError $
        ErrorWithPosition
            (errStr ++
             ". The numtype of 1st operand is " ++
             toReadableNT nt1 ++
             ", but the numtype of 2nd operand is " ++ toReadableNT nt2)
            pos

-- | Utils
--
anyJust :: [Maybe a] -> Maybe a
anyJust = firstJust id

getShape :: (ExpressionMap, Int) -> HE.Shape
getShape (mp, n) = HN.retrieveShape n mp

getNT :: (ExpressionMap, Int) -> HE.ET
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
