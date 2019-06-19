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
    | ElementHighest

data ShapeOutcome
    = ShapeSpecific Shape
    | ShapeHighest

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
                ShapeHighest -> highestShape exps
        elementType =
            case elementOutcome of
                ElementSpecific et -> et
                ElementHighest -> highestElementType exps
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
        , shapeOutcome = ShapeHighest
        , elementOutcome = ElementHighest
        }

--  where
--    elementType = highestElementType es
--    shape = highestShape es
--    node = Mul elementType . map snd $ es
--    mergedMap = foldl1 IM.union . map fst $ es
sumMany :: [(ExpressionMap, Int)] -> (ExpressionMap, Int)
sumMany =
    apply $
    OperationOption
        { op = OpManyElement Sum
        , shapeOutcome = ShapeHighest
        , elementOutcome = ElementHighest
        }
