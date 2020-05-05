{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

--{-# OPTIONS_GHC -W #-}

module Symphony.Symphony where

import AbsHashedLang
import Codec.Picture
import Control.Monad (when)
import Control.Monad.Except
import qualified Data.Array as Array
import Data.Complex
import Data.List (intercalate)
import Data.List.Extra (firstJust)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Tuple.HT (fst3)
import ErrM
import qualified HashedExpression.Internal.Expression as HE
import HashedExpression.Internal.Expression (ExpressionMap, Node (..))
import HashedExpression.Internal.Inner
import qualified HashedExpression.Internal.Node as HN
import HashedExpression.Internal.Utils
import qualified HashedExpression.Operation as HO
import HashedExpression.Prettify
import qualified HashedExpression.Solver as HS
import qualified HashedExpression.Value as HV
import LayoutHashedLang
import LexHashedLang
import ParHashedLang
import Symphony.Common
import Symphony.Exp
import Text.Regex.Posix

myLLexer :: String -> [Token]
myLLexer = resolveLayout True . myLexer

-------------------------------------------------------------------------------
-- Parse content of symphony file
-- Thanks to BNFC, we only need a simple call to `pProblem`
parse :: String -> Result Problem
parse fileContent =
  case pProblem . myLLexer $ fileContent of
    Ok problem -> return problem
    Bad errStr ->
      case (errStr =~ "line ([0-9]+), column ([0-9]+)" :: (String, String, String, [String])) of
        (_, _, _, [rs, cs]) ->
          -- because layout parsing add dummy ';'
          let r = min (read rs) (length r2c)
              c = min (read cs) (getNumColumn r)
           in throwError $ SyntaxError (r, c)
        _ -> throwError $ SyntaxError (1, 1)
  where
    r2c = Map.fromList . zip [1 ..] . map length . lines $ fileContent
    getNumColumn r = fromMaybe 0 $ Map.lookup r r2c

data ValidSymphony = ValidSymphony (ExpressionMap, Int) Vars Consts [HS.ConstraintStatement]

-------------------------------------------------------------------------------
-- 1. Check if there is variables block
-- 2. Check if variable block is valid (no name clash)
-- 3. If there is a constant, check if it is valid (no name clash)
-- 4. Check if all expressions (in constraints and objective) is valid (should be scalar, all operation should be valid),
--        if yes construct Expression.
-- 5. Gen code
checkSemantic :: Problem -> Result ValidSymphony
checkSemantic problem = do
  let toExp name (shape, _) = varWithShape shape name
  varDecls <- getVarDecls problem -- 1
  vars <- foldM checkVariableDecl Map.empty varDecls -- 2
  constDecls <- getConstDecls problem
  consts <- foldM (checkConstantDecl vars) Map.empty constDecls
  let initContext =
        Context
          { declarations =
              Map.mapWithKey toExp vars
                `Map.union` Map.mapWithKey toExp consts,
            vars = vars,
            consts = consts
          }
  letDecls <- getLetDecls problem
  finalContext <- foldM checkLetDecl initContext letDecls
  -- Complete processing context (variables, constants, declared values)
  -- Constraint
  constraintDecls <- getConstraintDecls problem
  css <- foldM (checkConstraintDecl finalContext) [] constraintDecls
  -- Objective
  parseObjectiveExp <- getMinimizeBlock problem
  objectiveExp <- constructExp finalContext (Just []) parseObjectiveExp
  let shape = getShape objectiveExp
      nt = getNT objectiveExp
  when (shape /= [] || nt /= HE.R)
    $ throwError
    $ ErrorWithPosition
      ( "Objective should be a real scalar, here it is ("
          ++ toReadable shape
          ++ ", "
          ++ show nt
          ++ ")"
      )
      (getBeginningPosition parseObjectiveExp)
  liftIO $ putStrLn "Syntax & semantic is correct"
  return $ ValidSymphony objectiveExp vars consts css

-------------------------------------------------------------------------------

-- | Generate code
generateCode :: String -> ValidSymphony -> Result ()
generateCode outputPath (ValidSymphony objectiveExp vars consts css) = do
  let problemGen =
        HS.constructProblem
          -- Objective
          (wrap objectiveExp)
          -- Variables
          (Map.keys vars)
          -- Constraints
          (HS.Constraint css)
  case problemGen of
    HS.ProblemValid heProblem -> do
      let valMap =
            Map.mapMaybeWithKey varVal vars
              `Map.union` Map.mapMaybeWithKey constVal consts
      case HS.generateProblemCode valMap heProblem of
        HS.Invalid reason -> throwError $ GeneralError reason
        HS.Success res -> liftIO $ res outputPath
    HS.ProblemInvalid reason -> throwError $ GeneralError reason
    HS.NoVariables -> throwError $ GeneralError "No variable to optimize over, feasibility problems are not supported yet"
  where
    varVal _ (_, Just val) = Just val
    varVal _ _ = Nothing
    constVal _ (_, val) = Just val

-------------------------------------------------------------------------------

-- | Get variable declarations, there must be exact 1 variables block
getVarDecls :: Problem -> Result [VariableDecl]
getVarDecls (Problem blocks) =
  case filter isVariableBlock blocks of
    [] -> throwError $ GeneralError "No variale block"
    [BlockVariable declss] -> return $ concat declss
    _ -> throwError $ GeneralError "There are more than 1 variables block"
  where
    isVariableBlock (BlockVariable declss) = True
    isVariableBlock _ = False

-------------------------------------------------------------------------------

-- | Get constants declarations, there must be at most 1 constants block
getConstDecls :: Problem -> Result [ConstantDecl]
getConstDecls (Problem blocks) =
  case filter isConstantBlock blocks of
    [] -> return []
    [BlockConstant declss] -> return $ concat declss
    _ -> throwError $ GeneralError "There are more than 1 constant block"
  where
    isConstantBlock (BlockConstant declss) = True
    isConstantBlock _ = False

-------------------------------------------------------------------------------

-- | Get constraints declarations, there must be at most 1 constraints block
getConstraintDecls :: Problem -> Result [ConstraintDecl]
getConstraintDecls (Problem blocks) =
  case filter isConstraintBlock blocks of
    [] -> return []
    [BlockConstraint declss] -> return $ concat declss
    _ -> throwError $ GeneralError "There are more than 1 constraint block"
  where
    isConstraintBlock (BlockConstraint declss) = True
    isConstraintBlock _ = False

-------------------------------------------------------------------------------

-- | Get immediate declarations, at most 1 block
getLetDecls :: Problem -> Result [LetDecl]
getLetDecls (Problem blocks) =
  case filter isLetBlock blocks of
    [] -> return []
    [BlockLet declss] -> return $ concat declss
    _ -> throwError $ GeneralError "There are more than 1 let block"
  where
    isLetBlock (BlockLet declss) = True
    isLetBlock _ = False

-------------------------------------------------------------------------------

-- | Get objective block, must be 1 block
getMinimizeBlock :: Problem -> Result Exp
getMinimizeBlock (Problem blocks) =
  case filter isMinimizeBlock blocks of
    [] -> throwError $ GeneralError "No minimize block"
    [BlockMinimize exp] -> return exp
    _ -> throwError $ GeneralError "There are more than 1 minimize block"
  where
    isMinimizeBlock (BlockMinimize declss) = True
    isMinimizeBlock _ = False

-------------------------------------------------------------------------------

-- | Check variable declarations
checkVariableDecl :: Vars -> VariableDecl -> Result Vars
checkVariableDecl accRes decl = do
  let checkName name pos =
        when (name `Map.member` accRes)
          $ throwError
          $ ErrorWithPosition ("Duplicate declaration of " ++ name) pos
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

-------------------------------------------------------------------------------

-- | Check constant declarations
checkConstantDecl :: Vars -> Consts -> ConstantDecl -> Result Consts
checkConstantDecl vars accRes decl = do
  let (ConstantDecl (PIdent (pos, name)) shape val) = decl
  when (name `Map.member` accRes)
    $ throwError
    $ ErrorWithPosition ("Duplicate declaration of " ++ name) pos
  when (name `Map.member` vars)
    $ throwError
    $ ErrorWithPosition (name ++ " already defined as variables") pos
  let heShape = toHEShape shape
  checkVal heShape val
  heVal <- toHEVal heShape val
  return $ Map.insert name (toHEShape shape, heVal) accRes

-------------------------------------------------------------------------------

-- | Check constraints declarations
checkConstraintDecl ::
  Context ->
  [HS.ConstraintStatement] ->
  ConstraintDecl ->
  Result [HS.ConstraintStatement]
checkConstraintDecl context@Context {..} acc decl = do
  (constructedExp, boundVal) <-
    case (isVariable exp, bound) of
      (Just varExp, ConstantBound (PIdent (pos, name)))
        | Just (shape, val) <- Map.lookup name consts -> do
          when (shape /= getShape varExp)
            $ throwError
            $ ErrorWithPosition
              "Shape mismatched: the bound doesn't have same shape as the variable"
              pos
          return (varExp, val)
        | otherwise ->
          throwError $ ErrorWithPosition (name ++ " not found") pos
      (Just varExp, NumberBound num) ->
        return (varExp, HV.VNum (numToDouble num))
      _ -> do
        scalarExp <- constructExp context Nothing exp
        when (getShape scalarExp /= [])
          $ throwError
          $ ErrorWithPosition
            ( "Higher-dimension (in)equality is not supported yet, here the expression has shape "
                ++ toReadable (getShape scalarExp)
            )
            (getBeginningPosition exp)
        case bound of
          ConstantBound (PIdent (pos, name))
            | Just (shape, val) <- Map.lookup name consts -> do
              when (shape /= getShape scalarExp)
                $ throwError
                $ ErrorWithPosition "The bound must be scalar" pos
              return (scalarExp, val)
            | otherwise ->
              throwError $
                ErrorWithPosition (name ++ " not found") pos
          NumberBound num ->
            return (scalarExp, HV.VNum (numToDouble num))
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
          | Just constructedExp <- Map.lookup name declarations,
            name `Map.member` vars ->
            Just constructedExp
        _ -> Nothing

-------------------------------------------------------------------------------

-- | Check immediate declarations
checkLetDecl :: Context -> LetDecl -> Result Context
checkLetDecl context@Context {..} (LetDecl (PIdent (pos, name)) exp) = do
  when (name `Map.member` consts)
    $ throwError
    $ ErrorWithPosition (name ++ " already defined as a constant") pos
  when (name `Map.member` vars)
    $ throwError
    $ ErrorWithPosition (name ++ " already defined as a variable") pos
  when (name `Map.member` declarations)
    $ throwError
    $ ErrorWithPosition (name ++ " already taken") pos
  constructedExp <- constructExp context Nothing exp
  let newDeclarations = Map.insert name constructedExp declarations
  return $ context {declarations = newDeclarations}

-------------------------------------------------------------------------------

-- | To HashedExpression's value
toHEVal :: HE.Shape -> Val -> Result HV.Val
toHEVal shape v =
  case v of
    ValFile filePath -> return $ HV.VFile $ HV.TXT filePath
    ValDataset filePath dataset ->
      return $ HV.VFile $ HV.HDF5 filePath dataset
    ValPattern (KWDataPattern pattern) ->
      case (pattern, shape) of
        ("FIRST_ROW_1", [size1, size2]) ->
          return
            . HV.V2D
            . Array.listArray ((0, 0), (size1 - 1, size2 - 1))
            $ replicate size2 1 ++ repeat 0
        ("LAST_ROW_1", [size1, size2]) ->
          return
            . HV.V2D
            . Array.listArray ((0, 0), (size1 - 1, size2 - 1))
            $ replicate (size2 * (size1 - 1)) 0 ++ repeat 1
        ("FIRST_COLUMN_1", [size1, size2]) ->
          return
            . HV.V2D
            . Array.listArray ((0, 0), (size1 - 1, size2 - 1))
            $ concat
            $ replicate size1
            $ 1 : replicate (size2 - 1) 0
        ("LAST_COLUMN_1", [size1, size2]) ->
          return
            . HV.V2D
            . Array.listArray ((0, 0), (size1 - 1, size2 - 1))
            $ concat
            $ replicate size1
            $ replicate (size2 - 1) 0 ++ [1]
        ("FIRST_ROW_0", [size1, size2]) ->
          return
            . HV.V2D
            . Array.listArray ((0, 0), (size1 - 1, size2 - 1))
            $ replicate size2 0 ++ repeat 1
        ("LAST_ROW_0", [size1, size2]) ->
          return
            . HV.V2D
            . Array.listArray ((0, 0), (size1 - 1, size2 - 1))
            $ replicate (size2 * (size1 - 1)) 1 ++ repeat 0
        ("FIRST_COLUMN_0", [size1, size2]) ->
          return
            . HV.V2D
            . Array.listArray ((0, 0), (size1 - 1, size2 - 1))
            $ concat
            $ replicate size1
            $ 0 : replicate (size2 - 1) 1
        ("LAST_COLUMN_0", [size1, size2]) ->
          return
            . HV.V2D
            . Array.listArray ((0, 0), (size1 - 1, size2 - 1))
            $ concat
            $ replicate size1
            $ replicate (size2 - 1) 1 ++ [0]
        _ ->
          throwError
            $ GeneralError
            $ "Pattern "
              ++ pattern
              ++ " is incompatible with the shape or not supported yet"
    ValRandom -> return $ HV.VNum 3
    ValLiteral num -> return $ HV.VNum (numToDouble num)
    ValImage imgPath -> do
      a <- liftIO $ readImage imgPath
      case a of
        Left err -> throwError $ GeneralError $ "Error reading image at " <> imgPath <> ":" <> err
        Right v -> do
          -- TODO : only support grayscale yet, and this is very slow
          let img = convertRGB8 v
          let col = imageWidth img
              row = imageHeight img
              toGrayscale :: Pixel8 -> Pixel8 -> Pixel8 -> Double
              toGrayscale r g b = (0.2126 * (fromIntegral r) + 0.7152 * (fromIntegral g) + 0.0722 * (fromIntegral b)) / 256
          if (shape /= [row, col])
            then throwError $ GeneralError $ "image size and variable shape don't match, image size is " ++ show row ++ "x" ++ show col
            else
              return $ HV.V2D $
                Array.listArray
                  ((0, 0), (row - 1, col - 1))
                  [ toGrayscale r g b
                    | i <- [0 .. row - 1],
                      j <- [0 .. col - 1],
                      let (PixelRGB8 r g b) = pixelAt img j i
                  ]
