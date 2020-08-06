module Symphony.Exp where

import AbsHashedLang
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except
import Data.List (intercalate)
import Data.List.Extra (firstJust)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import HashedExpression.Internal
import HashedExpression.Internal.Expression (ExpressionMap, NodeID, Op (..))
import qualified HashedExpression.Internal.Expression as HE
import qualified HashedExpression.Internal.Node as HN
import HashedExpression.Internal.OperationSpec
import qualified HashedExpression.Internal.Utils as HU
import qualified HashedExpression.Operation as HO
import Symphony.Common

-------------------------------------------------------------------------------

-- | Infer the shape of expression with given context
inferShape :: Context -> Exp -> Maybe HE.Shape
inferShape context@Context {..} exp =
  case exp of
    ENumDouble _ -> Nothing
    ENumInteger _ -> Nothing
    EIdent (PIdent (_, name))
      | Just exp <- Map.lookup name declarations -> Just $ getShape exp
      | otherwise -> Nothing
    EPlus exp1 _ exp2 -> firstJust (inferShape context) [exp1, exp2]
    ERealImag exp1 _ exp2 -> firstJust (inferShape context) [exp1, exp2]
    ESubtract exp1 _ exp2 -> firstJust (inferShape context) [exp1, exp2]
    EMul exp1 _ exp2 -> firstJust (inferShape context) [exp1, exp2]
    EDiv exp1 _ exp2 -> firstJust (inferShape context) [exp1, exp2]
    EScale exp1 _ exp2 -> firstJust (inferShape context) [exp1, exp2]
    EDot exp1 _ exp2 -> Just []
    EPower exp1 _ val -> firstJust (inferShape context) [exp1]
    ERotate (TokenRotate _) rotateAmount exp -> firstJust (inferShape context) [exp]
    ENegate (TokenSub _) exp -> firstJust (inferShape context) [exp]
    EPiecewise (TokenCase _) exp cases -> firstJust (inferShape context) [exp]
    EUnaryFun (PUnaryFun _) exp -> firstJust (inferShape context) [exp] -- TODO: depends on PIdent
    EDoubleFun (PDoubleFun _) _ exp -> firstJust (inferShape context) [exp] -- TODO: depends on PIdent

-------------------------------------------------------------------------------

-- | Construct a hashed expression given context and inferred shape
constructExp :: Context -> Maybe HE.Shape -> Exp -> Result (ExpressionMap, NodeID)
constructExp context shapeInfo exp =
  let add x y = sumMany [x, y]
      multiply x y = mulMany [x, y]
      reIm x y = apply (Binary specRealImag) [x, y]
      scale x y = apply (Binary specScale) [x, y]
      subtract x y = sumMany [x, scale (HU.aConst [] (-1)) y]
      dot x y = apply (Binary specInnerProd) [x, y]
      app fun x = apply (Unary fun) [x]
      piecewise marks condition branches =
        apply (ConditionAry (specPiecewise marks)) (condition : branches)
   in case exp of
        ENumDouble (PDouble (pos, valStr))
          | Just shape <- shapeInfo -> return $ HU.aConst shape (read valStr)
          | otherwise -> throwError $ ErrorWithPosition ("Ambiguous shape of literal " ++ valStr) pos
        ENumInteger (PInteger (pos, valStr))
          | Just shape <- shapeInfo -> return $ HU.aConst shape (read valStr)
          | otherwise -> throwError $ ErrorWithPosition ("Ambiguous shape of literal " ++ valStr) pos
        EIdent (PIdent (idPos, name)) -> retrieveExpFromIdent context (idPos, name)
        EPlus exp1 (TokenPlus (opPos, _)) exp2 -> do
          let inferredShape = inferShape context exp1 <|> inferShape context exp2 <|> shapeInfo
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
                inferShape context exp1 <|> inferShape context exp2 <|> shapeInfo
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
          let inferredShape = inferShape context exp1 <|> inferShape context exp2 <|> shapeInfo
          operand1 <- constructExp context inferredShape exp1
          operand2 <- constructExp context inferredShape exp2
          checkSameShape operand1 operand2 "Shape mismatched: trying to subtract 2 vectors with different shape" opPos
          checkSameNumType operand1 operand2 "Numtype mismatched: trying to subtract 2 vectors with different numtype" opPos
          return $ add operand1 (scale (HU.aConst [] (-1)) operand2)
        EMul exp1 (TokenMul (opPos, _)) exp2 -> do
          let inferredShape = inferShape context exp1 <|> inferShape context exp2 <|> shapeInfo
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
          let inferredShape = inferShape context exp1 <|> inferShape context exp2 <|> shapeInfo
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
          return $ multiply operand1 (app (specPower (-1)) operand2)
        EScale exp1 (TokenScale (opPos, _)) exp2 -> do
          let inferredShape = inferShape context exp1 <|> shapeInfo
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
                inferShape context exp1 <|> inferShape context exp2
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
          return $ app (specPower (toInt val)) operand1
        ERotate (TokenRotate (opPos, _)) raStr exp -> do
          operand <- constructExp context shapeInfo exp
          let rotateAmount = toRotateAmount raStr
          let shape = getShape operand
          when (null shape) $
            throwError $
              ErrorWithPosition "Can't rotate a scalar" opPos
          when (length shape /= length rotateAmount) $
            throwError $
              ErrorWithPosition
                ( "The rotate amount is for "
                    ++ show (length rotateAmount)
                    ++ "d vector, but the operand is "
                    ++ show (length shape)
                    ++ "d vector"
                )
                opPos
          return $ app (specRotate rotateAmount) operand
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
                  ( ". The shape of condition is "
                      ++ toReadable shape
                      ++ ", but the shape of branch "
                      ++ show idx
                      ++ " is "
                      ++ toReadable (getShape e)
                  )
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
          let singleArgReal op = do
                onlyForRealVector operand
                return $ app op operand
          case funName of
            "sqrt" -> singleArgReal specSqrt
            "sin" -> singleArgReal specSin
            "cos" -> singleArgReal specCos
            "tan" -> singleArgReal specTan
            "exp" -> singleArgReal specExp
            "log" -> singleArgReal specLog
            "sinh" -> singleArgReal specSinh
            "cosh" -> singleArgReal specCosh
            "tanh" -> singleArgReal specTanh
            "asin" -> singleArgReal specAsin
            "acos" -> singleArgReal specAcos
            "atan" -> singleArgReal specAtan
            "asinh" -> singleArgReal specAsin
            "acosh" -> singleArgReal specAcos
            "atanh" -> singleArgReal specAtan
            "xRe" -> do
              onlyForComplexVector operand
              return $ app specRealPart operand
            "xIm" -> do
              onlyForComplexVector operand
              return $ app specImagPart operand
            "ft" -> do
              onlyForComplexVector operand
              return $ app specFT operand
            "sumElements" -> do
              onlyForRealVector operand
              return $ dot operand (HU.aConst (getShape operand) 1)
            "norm2square" -> do
              operand <- constructExp context shapeInfo exp
              if getNT operand == HE.R
                then return $ dot operand operand
                else do
                  let re = app specRealPart operand
                      im = app specImagPart operand
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
          operand <- constructExp context shapeInfo exp
          let delta = numToDouble num
          let huber delta operand =
                let one = HU.aConst (getShape operand) 1
                    const val = HU.aConst [] val
                    inner = const 0.5 `scale` (operand `multiply` operand)
                    outerLeft = (const (- delta) `scale` operand) `subtract` (const (delta * delta / 2) `scale` one)
                    outerRight = (const delta `scale` operand) `subtract` (const (delta * delta / 2) `scale` one)
                 in piecewise [- delta, delta] operand [outerLeft, inner, outerRight]
          case name of
            "huber" -> do
              onlyForRealVector operand
              return $ huber delta operand
            "normHuber" -> do
              onlyForRealVector operand
              return $ dot (huber delta operand) (HU.aConst (getShape operand) 1)
            _ ->
              throwError $ ErrorWithPosition "Function not found" funPos

-------------------------------------------------------------------------------

-- | Check if two expression have the same shape
checkSameShape ::
  (ExpressionMap, NodeID) ->
  (ExpressionMap, NodeID) ->
  String ->
  (Int, Int) ->
  Result ()
checkSameShape operand1 operand2 errStr pos = do
  let shape1 = getShape operand1
      shape2 = getShape operand2
  unless (shape1 == shape2) $
    throwError $
      ErrorWithPosition
        ( errStr
            ++ ". The shape of 1st operand is "
            ++ toReadable shape1
            ++ ", but the shape of 2nd operand 2 is "
            ++ toReadable shape2
        )
        pos

-- | Check if two expression have the same num type
checkSameNumType ::
  (ExpressionMap, NodeID) ->
  (ExpressionMap, NodeID) ->
  String ->
  (Int, Int) ->
  Result ()
checkSameNumType operand1 operand2 errStr pos = do
  let nt1 = getNT operand1
      nt2 = getNT operand2
  unless (nt1 == nt2) $
    throwError $
      ErrorWithPosition
        ( errStr
            ++ ". The numtype of 1st operand is "
            ++ toReadableNT nt1
            ++ ", but the numtype of 2nd operand is "
            ++ toReadableNT nt2
        )
        pos
