-------------------------------------------------------------------------------
-- | Inner Hashed functions, there are functions as
-- how to combine expression with options
--
--
-------------------------------------------------------------------------------
module HashedInner where

import qualified Data.IntMap.Strict as IM
import Data.List (sort, sortBy, sortOn)
import HashedExpression
import HashedHash
import HashedNode
import HashedUtils

-- |
--
data ElementOutcome
    = ElementSpecific ET
    | ElementDefault -- Highest element (R < C < Covector)

data NodeOutcome
    = OpOne (Arg -> Node)
    | OpOneElement (ET -> Arg -> Node) ElementOutcome
    | OpTwo (Arg -> Arg -> Node)
    | OpTwoElement (ET -> Arg -> Arg -> Node) ElementOutcome
    | OpMany (Args -> Node)
    | OpManyElement (ET -> Args -> Node) ElementOutcome

data ShapeOutcome
    = ShapeSpecific Shape
    | ShapeDefault -- Highest shape (shape with longest length)

data OperationOption =
    OperationOption
        { nodeOutcome :: NodeOutcome
        , shapeOutcome :: ShapeOutcome
        }

-- |
--
unwrap :: Expression d et -> (ExpressionMap, Int)
unwrap (Expression n mp) = (mp, n)

wrap :: (ExpressionMap, Int) -> Expression d et
wrap = uncurry $ flip Expression

-- |
--
highestShape :: [(ExpressionMap, Int)] -> Shape
highestShape = last . sortOn length . map (uncurry $ flip retrieveShape)

-- | R < C < Covector
--
highestElementType :: [(ExpressionMap, Int)] -> ET
highestElementType = maximum . map (uncurry $ flip retrieveElementType)

-- | The apply function that is used everywhere
--
apply :: OperationOption -> [(ExpressionMap, Int)] -> (ExpressionMap, Int)
apply (OperationOption nodeOutcome shapeOutcome) exps =
    let mergedMap = foldl1 IM.union . map fst $ exps
        shape =
            case shapeOutcome of
                ShapeSpecific s -> s
                ShapeDefault -> highestShape exps
        elementType elementOutcome =
            case elementOutcome of
                ElementSpecific et -> et
                ElementDefault -> highestElementType exps
        node =
            case (nodeOutcome, map snd exps) of
                (OpOne op, [arg]) -> op arg
                (OpOneElement op elm, [arg]) -> op (elementType elm) arg
                (OpTwo op, [arg1, arg2]) -> op arg1 arg2
                (OpTwoElement op elm, [arg1, arg2]) ->
                    op (elementType elm) arg1 arg2
                (OpMany op, args) -> op args
                (OpManyElement op elm, args) -> op (elementType elm) args
                _ -> error "HashedInner.apply"
     in addEntry mergedMap (shape, node)

-- | General multiplication and sum
--
mulMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
mulMany = apply $ naryET Mul ElementDefault

sumMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
sumMany = apply $ naryET Sum ElementDefault

-- |
--
hasShape :: OperationOption -> Shape -> OperationOption
hasShape (OperationOption nodeOutcome _) specificShape =
    OperationOption nodeOutcome (ShapeSpecific specificShape)

applyBinary ::
       OperationOption
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
applyBinary option e1 e2 = wrap . apply option $ [unwrap e1, unwrap e2]

applyUnary :: OperationOption -> Expression d1 et1 -> Expression d2 et2
applyUnary option e1 = wrap . apply option $ [unwrap e1]

applyNary :: OperationOption -> [Expression d1 et1] -> Expression d2 et2
applyNary option = wrap . apply option . map unwrap

-- | binary operations
--
binary :: (Arg -> Arg -> Node) -> OperationOption
binary op =
    OperationOption {nodeOutcome = OpTwo op, shapeOutcome = ShapeDefault}

binaryET :: (ET -> Arg -> Arg -> Node) -> ElementOutcome -> OperationOption
binaryET op elm =
    OperationOption
        {nodeOutcome = OpTwoElement op elm, shapeOutcome = ShapeDefault}

-- | unary operations
--
unary :: (Arg -> Node) -> OperationOption
unary op = OperationOption {nodeOutcome = OpOne op, shapeOutcome = ShapeDefault}

unaryET :: (ET -> Arg -> Node) -> ElementOutcome -> OperationOption
unaryET op elm =
    OperationOption
        {nodeOutcome = OpOneElement op elm, shapeOutcome = ShapeDefault}

-- | n-ary operations
--
nary :: (Args -> Node) -> OperationOption
nary op = OperationOption {nodeOutcome = OpMany op, shapeOutcome = ShapeDefault}

naryET :: (ET -> Args -> Node) -> ElementOutcome -> OperationOption
naryET op elm =
    OperationOption
        {nodeOutcome = OpManyElement op elm, shapeOutcome = ShapeDefault}

-- | Reconstruct
--
reconstruct ::
       (ExpressionMap, Int) -> [(ExpressionMap, Int)] -> (ExpressionMap, Int)
reconstruct oldExp@(oldMp, oldN) newChildren =
    let (oldShape, oldNode) = retrieveInternal oldN oldMp
        apply' option = apply (option `hasShape` oldShape) -- keep the old shape
     in case oldNode of
            Var _ -> oldExp
            DVar _ -> oldExp
            Const _ -> oldExp
            Sum et _ -> apply' (naryET Sum (ElementSpecific et)) newChildren
            Mul et _ -> apply' (naryET Mul (ElementSpecific et)) newChildren
            Neg et _ -> apply' (unaryET Neg (ElementSpecific et)) newChildren
            Scale et _ _ ->
                apply' (binaryET Scale (ElementSpecific et)) newChildren
            Div _ _ -> apply' (binary Div) newChildren
            Sqrt _ -> apply' (unary Sqrt) newChildren
            Sin _ -> apply' (unary Sin) newChildren
            Cos _ -> apply' (unary Cos) newChildren
            Tan _ -> apply' (unary Tan) newChildren
            Exp _ -> apply' (unary Exp) newChildren
            Log _ -> apply' (unary Log) newChildren
            Sinh _ -> apply' (unary Sinh) newChildren
            Cosh _ -> apply' (unary Cosh) newChildren
            Tanh _ -> apply' (unary Tanh) newChildren
            Asin _ -> apply' (unary Asin) newChildren
            Acos _ -> apply' (unary Acos) newChildren
            Atan _ -> apply' (unary Atan) newChildren
            Asinh _ -> apply' (unary Asinh) newChildren
            Acosh _ -> apply' (unary Acosh) newChildren
            Atanh _ -> apply' (unary Atanh) newChildren
            RealImag _ _ -> apply' (binary RealImag) newChildren
            RealPart _ -> apply' (unary RealPart) newChildren
            ImagPart _ -> apply' (unary ImagPart) newChildren
            InnerProd et _ _ ->
                apply' (binaryET InnerProd (ElementSpecific et)) newChildren
