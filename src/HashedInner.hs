-------------------------------------------------------------------------------
-- | Inner Hashed functions, there are functions as
-- how to combine expression with options
--
--
-------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

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

-- | R < C < Covector -- TODO: Is this right?
--
highestElementType :: [(ExpressionMap, Int)] -> ET
highestElementType = maximum . map (uncurry $ flip retrieveElementType)

-- | The ultimate apply function that is used by many places
--
apply :: OperationOption -> [(ExpressionMap, Int)] -> (ExpressionMap, Int)
apply OperationOption {..} exps =
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
     in addEdge mergedMap (shape, node)

-- | General multiplication and sum
--
mulMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
mulMany =
    apply $
    OperationOption
        {nodeOutcome = OpManyElement Mul ElementDefault, shapeOutcome = ShapeDefault}

sumMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
sumMany =
    apply $
    OperationOption
        {nodeOutcome = OpManyElement Sum ElementDefault, shapeOutcome = ShapeDefault}

-- | More helpers
--
binary ::
       (Arg -> Arg -> Node)
    -> ShapeOutcome
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
binary o s e1 e2 = undefined
--    wrap . apply (OperationOption (OpTwo o) ElementDefault s) $
--    [unwrap e1, unwrap e2]

-- |
--
binaryET ::
       (ET -> Arg -> Arg -> Node)
    -> ElementOutcome
    -> ShapeOutcome
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
binaryET o elm s e1 e2 = undefined
--    wrap . apply (OperationOption (OpTwoElement o) elm s) $
--    [unwrap e1, unwrap e2]

-- |
--
monory :: (Arg -> Node) -> Expression d et1 -> Expression d et2
monory op ex = undefined
--    wrap . apply (OperationOption (OpOne op) ElementDefault ShapeDefault) $
--    [unwrap ex]

-- |
--
monoryET :: (ET -> Arg -> Node) -> Expression d et1 -> Expression d et2
monoryET op ex = undefined
--    wrap . apply (OperationOption (OpOneElement op) ElementDefault ShapeDefault) $
--    [unwrap ex]

unwrapBinary ::
       (Arg -> Arg -> Node)
    -> ShapeOutcome
    -> (ExpressionMap, Int)
    -> (ExpressionMap, Int)
    -> (ExpressionMap, Int)
unwrapBinary o s e1 e2 = undefined
--    apply (OperationOption (OpTwo o) ElementDefault s) [e1, e2]

-- |
--
unwrapBinaryET ::
       (ET -> Arg -> Arg -> Node)
    -> ElementOutcome
    -> ShapeOutcome
    -> (ExpressionMap, Int)
    -> (ExpressionMap, Int)
    -> (ExpressionMap, Int)
unwrapBinaryET o elm s e1 e2 = undefined
--    apply (OperationOption (OpTwoElement o) elm s) [e1, e2]

-- |
--
unwrapMonory :: (Arg -> Node) -> (ExpressionMap, Int) -> (ExpressionMap, Int)
unwrapMonory op ex = undefined
--    apply (OperationOption (OpOne op) ElementDefault ShapeDefault) [ex]

-- |
--
unwrapMonoryET ::
       (ET -> Arg -> Node) -> (ExpressionMap, Int) -> (ExpressionMap, Int)
unwrapMonoryET op ex = undefined
--    apply (OperationOption (OpOneElement op) ElementDefault ShapeDefault) [ex]
