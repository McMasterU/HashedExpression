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

data Op
    = OpOne (Arg -> Node)
    | OpOneElement (ET -> Arg -> Node)
    | OpTwo (Arg -> Arg -> Node)
    | OpTwoElement (ET -> Arg -> Arg -> Node)
    | OpMany (Args -> Node)
    | OpManyElement (ET -> Args -> Node)

data ElementOutcome
    = ElementSpecific ET
    | ElementDefault -- Highest element (R < C < Covector)

data ShapeOutcome
    = ShapeSpecific Shape
    | ShapeDefault -- Highest shape (shape with longest length)

data OperationOption =
    OperationOption
        { op :: Op
        , elementOutcome :: ElementOutcome
        , shapeOutcome :: ShapeOutcome
        }

-- | The ultimate apply function that is used by many places
--
apply :: OperationOption -> [(ExpressionMap, Int)] -> (ExpressionMap, Int)
apply OperationOption {..} exps =
    let mergedMap = foldl1 IM.union . map fst $ exps
        shape =
            case shapeOutcome of
                ShapeSpecific s -> s
                ShapeDefault -> highestShape exps
        elementType =
            case elementOutcome of
                ElementSpecific et -> et
                ElementDefault -> highestElementType exps
        node =
            case (op, map snd exps) of
                (OpOne op, [arg]) -> op arg
                (OpOneElement op, [arg]) -> op elementType arg
                (OpTwo op, [arg1, arg2]) -> op arg1 arg2
                (OpTwoElement op, [arg1, arg2]) -> op elementType arg1 arg2
                (OpMany op, args) -> op args
                (OpManyElement op, args) -> op elementType args
     in addEdge mergedMap (shape, node)

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

-- | General multiplication and sum
--
mulMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
mulMany =
    apply $
    OperationOption
        { op = OpManyElement Mul
        , shapeOutcome = ShapeDefault
        , elementOutcome = ElementDefault
        }

sumMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
sumMany =
    apply $
    OperationOption
        { op = OpManyElement Sum
        , shapeOutcome = ShapeDefault
        , elementOutcome = ElementDefault
        }

-- | More helpers
--
binary ::
       (Arg -> Arg -> Node)
    -> ShapeOutcome
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
binary o s e1 e2 =
    wrap . apply (OperationOption (OpTwo o) ElementDefault s) $
    [unwrap e1, unwrap e2]

-- |
--
binaryET ::
       (ET -> Arg -> Arg -> Node)
    -> ElementOutcome
    -> ShapeOutcome
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
binaryET o elm s e1 e2 =
    wrap . apply (OperationOption (OpTwoElement o) elm s) $
    [unwrap e1, unwrap e2]

-- |
--
monory :: (Arg -> Node) -> Expression d et1 -> Expression d et2
monory op ex =
    wrap . apply (OperationOption (OpOne op) ElementDefault ShapeDefault) $
    [unwrap ex]

-- |
--
monoryET :: (ET -> Arg -> Node) -> Expression d et1 -> Expression d et2
monoryET op ex =
    wrap . apply (OperationOption (OpOneElement op) ElementDefault ShapeDefault) $
    [unwrap ex]

unwrapBinary ::
       (Arg -> Arg -> Node)
    -> ShapeOutcome
    -> (ExpressionMap, Int)
    -> (ExpressionMap, Int)
    -> (ExpressionMap, Int)
unwrapBinary o s e1 e2 =
    apply (OperationOption (OpTwo o) ElementDefault s) [e1, e2]

-- |
--
unwrapBinaryET ::
       (ET -> Arg -> Arg -> Node)
    -> ElementOutcome
    -> ShapeOutcome
    -> (ExpressionMap, Int)
    -> (ExpressionMap, Int)
    -> (ExpressionMap, Int)
unwrapBinaryET o elm s e1 e2 =
    apply (OperationOption (OpTwoElement o) elm s) [e1, e2]

-- |
--
unwrapMonory :: (Arg -> Node) -> (ExpressionMap, Int) -> (ExpressionMap, Int)
unwrapMonory op ex =
    apply (OperationOption (OpOne op) ElementDefault ShapeDefault) [ex]

-- |
--
unwrapMonoryET ::
       (ET -> Arg -> Node) -> (ExpressionMap, Int) -> (ExpressionMap, Int)
unwrapMonoryET op ex =
    apply (OperationOption (OpOneElement op) ElementDefault ShapeDefault) [ex]
