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
     in addEdge mergedMap (shape, node)

-- | General multiplication and sum
--
mulMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
mulMany =
    apply $
    OperationOption
        { nodeOutcome = OpManyElement Mul ElementDefault
        , shapeOutcome = ShapeDefault
        }

sumMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
sumMany =
    apply $
    OperationOption
        { nodeOutcome = OpManyElement Sum ElementDefault
        , shapeOutcome = ShapeDefault
        }

-- |
--
hasShape :: OperationOption -> Shape -> OperationOption
hasShape (OperationOption nodeOutcome _) specificShape =
    OperationOption nodeOutcome (ShapeSpecific specificShape)

opBinary ::
       OperationOption
    -> Expression d1 et1
    -> Expression d2 et2
    -> Expression d3 et3
opBinary option e1 e2 = wrap . apply option $ [unwrap e1, unwrap e2]

opMonory :: OperationOption -> Expression d1 et1 -> Expression d2 et2
opMonory option e1 = wrap . apply option $ [unwrap e1]

-- |
--
binary :: (Arg -> Arg -> Node) -> OperationOption
binary op =
    OperationOption {nodeOutcome = OpTwo op, shapeOutcome = ShapeDefault}

binaryET :: (ET -> Arg -> Arg -> Node) -> ElementOutcome -> OperationOption
binaryET op elm =
    OperationOption
        {nodeOutcome = OpTwoElement op elm, shapeOutcome = ShapeDefault}

monory :: (Arg -> Node) -> OperationOption
monory op =
    OperationOption {nodeOutcome = OpOne op, shapeOutcome = ShapeDefault}

monoryET :: (ET -> Arg -> Node) -> ElementOutcome -> OperationOption
monoryET op elm =
    OperationOption
        {nodeOutcome = OpOneElement op elm, shapeOutcome = ShapeDefault}

multiry :: (Args -> Node) -> OperationOption
multiry op =
    OperationOption {nodeOutcome = OpMany op, shapeOutcome = ShapeDefault}

multiryET :: (ET -> Args -> Node) -> ElementOutcome -> OperationOption
multiryET op elm =
    OperationOption
        {nodeOutcome = OpManyElement op elm, shapeOutcome = ShapeDefault}
