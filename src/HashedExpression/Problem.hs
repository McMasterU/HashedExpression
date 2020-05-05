module HashedExpression.Problem where

import HashedExpression.Internal.Expression
import HashedExpression.Internal.Utils
import HashedExpression.Value

-- |
data Variable
  = Variable
      { varName :: String,
        nodeId :: Int,
        partialDerivativeId :: Int
      }
  deriving (Show)

-- |
data Problem
  = Problem
      { variables :: [Variable],
        objectiveId :: Int,
        expressionMap :: ExpressionMap,
        boxConstraints :: [BoxConstraint],
        scalarConstraints :: [ScalarConstraint]
      }

-- |
data BoxConstraint
  = BoxUpper String Val
  | BoxLower String Val
  | BoxBetween String (Val, Val)

-- |
data ScalarConstraint
  = ScalarConstraint
      { constraintValueId :: Int,
        constraintPartialDerivatives :: [Int],
        constraintLowerBound :: Double,
        constraintUpperBound :: Double
      }
  deriving (Show, Eq, Ord)

-- | 
data Constraint
  = NoConstraint
  | Constraint [ConstraintStatement]
  deriving (Show, Eq, Ord)

-- |
data ConstraintStatement
  = Lower (ExpressionMap, Int) Val
  | Upper (ExpressionMap, Int) Val
  | Between (ExpressionMap, Int) (Val, Val)
  deriving (Show, Eq, Ord)
  
