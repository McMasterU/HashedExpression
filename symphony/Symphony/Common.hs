module Symphony.Common where

import AbsHashedLang
import Control.Monad (when)
import Control.Monad.Except
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified HashedExpression.Internal.Expression as HE
import HashedExpression.Internal.Expression (ExpressionMap, Node (..), NodeID)
import HashedExpression.Internal.Inner
import qualified HashedExpression.Internal.Node as HN
import qualified HashedExpression.Operation as HO
import qualified HashedExpression.Value as HV

data CompileError
  = SyntaxError (Int, Int)
  | GeneralError String
  | ErrorWithPosition String (Int, Int)
  deriving (Show)

type Result a = ExceptT CompileError IO a

-- | (name, shape, initialize value)
type Vars = Map String (HE.Shape, Maybe HV.Val)

-- | (name, shape, value)
type Consts = Map String (HE.Shape, HV.Val)

data Context
  = Context
      { declarations :: Map String (ExpressionMap, NodeID),
        vars :: Vars,
        consts :: Consts
      }

-- | Utils
getShape :: (ExpressionMap, NodeID) -> HE.Shape
getShape (mp, n) = HN.retrieveShape n mp

getNT :: (ExpressionMap, NodeID) -> HE.ET
getNT (mp, n) = HN.retrieveElementType n mp

toReadable :: HE.Shape -> String
toReadable [] = "scalar"
toReadable xs = intercalate "x" . map show $ xs

toReadableNT :: HE.ET -> String
toReadableNT HE.R = "real"
toReadableNT HE.C = "complex"

-- | Helpers, convert between parse type and HashedExpression type
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
    IntNeg _ (PInteger (_, strVal)) -> - (read strVal)

toDouble :: TDouble -> Double
toDouble i =
  case i of
    DoublePos (PDouble (_, strVal)) -> read strVal
    DoubleNeg _ (PDouble (_, strVal)) -> - (read strVal)

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

retrieveExpFromIdent ::
  Context -> ((Int, Int), String) -> Result (ExpressionMap, NodeID)
retrieveExpFromIdent context@Context {..} (pos, name)
  | Just exp <- Map.lookup name declarations = return exp
  | otherwise = throwError $ ErrorWithPosition (name ++ " is undefined") pos

-- |  TODO: Check if val is valid w.r.t shape
checkVal :: HE.Shape -> Val -> Result ()
checkVal shape val = return ()
