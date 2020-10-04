{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      :  HashedExpression.Internal.OperationSpec
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This modules contains specification for all operations (each corresponding to a constructor of @Op)
module HashedExpression.Internal.OperationSpec where

import Data.Function ((&))
import Data.List (sort)
import GHC.Stack (HasCallStack)
import HashedExpression.Internal.Expression
import HashedExpression.Utils

data UnarySpec = UnarySpec
  { toOp :: Arg -> Op,
    decideShape :: Shape -> Shape,
    decideET :: ElementType -> ElementType
  }

data BinarySpec = BinarySpec
  { toOp :: Arg -> Arg -> Op,
    decideShape :: Shape -> Shape -> Shape,
    decideET :: ElementType -> ElementType -> ElementType
  }

data NarySpec = NarySpec
  { toOp :: [Arg] -> Op,
    decideShape :: [Shape] -> Shape,
    decideET :: [ElementType] -> ElementType
  }

data ConditionarySpec = ConditionarySpec
  { toOp :: Arg -> [Arg] -> Op,
    decideShape :: Shape -> [Shape] -> Shape,
    decideET :: ElementType -> [ElementType] -> ElementType
  }

data OperationSpec
  = Unary UnarySpec
  | Binary BinarySpec
  | Nary NarySpec
  | ConditionAry ConditionarySpec

-------------------------------------------------------------------------------
assertSame :: (HasCallStack, Ord a, Show a) => [a] -> b -> b
assertSame xs y
  | allEqual xs = y
  | otherwise = error $ "must be equal " ++ show xs

-------------------------------------------------------------------------------

-- |
defaultUnary :: HasCallStack => (Arg -> Op) -> [ElementType] -> UnarySpec
defaultUnary f allowedETs = UnarySpec {toOp = f, decideShape = id, decideET = decideET}
  where
    decideET et
      | et `elem` allowedETs = et
      | otherwise = error "Element type is not allowed"

-- |
defaultBinary :: HasCallStack => (Arg -> Arg -> Op) -> [ElementType] -> BinarySpec
defaultBinary f allowedETs = BinarySpec {toOp = f, decideShape = req, decideET = decideET}
  where
    req x y = assertSame [x, y] x
    decideET x y
      | x `elem` allowedETs = assertSame [x, y] x
      | otherwise = error "Element type is not allowed"

-------------------------------------------------------------------------------

specSum :: HasCallStack => NarySpec
specSum =
  NarySpec
    { toOp = Sum,
      decideShape = \xs -> assertSame xs $ head xs,
      decideET = \xs -> assertSame xs $ head xs
    }

specMul :: HasCallStack => NarySpec
specMul =
  NarySpec
    { toOp = Mul,
      decideShape = \xs -> assertSame xs $ head xs,
      decideET = \xs -> assertSame xs $ head xs
    }

specPower :: HasCallStack => Int -> UnarySpec
specPower alpha = defaultUnary (Power alpha) [R, C]

specNeg :: HasCallStack => UnarySpec
specNeg = defaultUnary Neg [R, C]

specScale :: HasCallStack => BinarySpec
specScale =
  BinarySpec
    { toOp = Scale,
      decideShape = decideShape,
      decideET = decideET
    }
  where
    decideShape x y
      | null x = y
      | otherwise = error "First operand must be scalar"
    decideET :: ElementType -> ElementType -> ElementType
    decideET R R = R
    decideET R C = C
    decideET C C = C
    decideET x y = error $ "Scaling invalid et " ++ show x ++ " " ++ show y

specDiv :: HasCallStack => BinarySpec
specDiv = defaultBinary Div [R]

specSqrt :: HasCallStack => UnarySpec
specSqrt = defaultUnary Sqrt [R]

specSin :: HasCallStack => UnarySpec
specSin = defaultUnary Sin [R]

specCos :: HasCallStack => UnarySpec
specCos = defaultUnary Cos [R]

specTan :: HasCallStack => UnarySpec
specTan = defaultUnary Tan [R]

specExp :: HasCallStack => UnarySpec
specExp = defaultUnary Exp [R]

specLog :: HasCallStack => UnarySpec
specLog = defaultUnary Log [R]

specSinh :: HasCallStack => UnarySpec
specSinh = defaultUnary Sinh [R]

specCosh :: HasCallStack => UnarySpec
specCosh = defaultUnary Cosh [R]

specTanh :: HasCallStack => UnarySpec
specTanh = defaultUnary Tanh [R]

specAsin :: HasCallStack => UnarySpec
specAsin = defaultUnary Asin [R]

specAcos :: HasCallStack => UnarySpec
specAcos = defaultUnary Acos [R]

specAtan :: HasCallStack => UnarySpec
specAtan = defaultUnary Atan [R]

specAsinh :: HasCallStack => UnarySpec
specAsinh = defaultUnary Asinh [R]

specAcosh :: HasCallStack => UnarySpec
specAcosh = defaultUnary Acosh [R]

specAtanh :: HasCallStack => UnarySpec
specAtanh = defaultUnary Atanh [R]

specRealImag :: HasCallStack => BinarySpec
specRealImag =
  BinarySpec {toOp = RealImag, decideShape = decideShape, decideET = decideET}
  where
    decideShape x y = assertSame [x, y] x
    decideET x y
      | x == R && y == R = C
      | otherwise = error $ "2 operands must be real" ++ show [x, y]

specRealPart :: HasCallStack => UnarySpec
specRealPart =
  UnarySpec {toOp = RealPart, decideShape = id, decideET = decideET}
  where
    decideET x
      | x == C = R
      | otherwise = error "Must be complex"

specImagPart :: HasCallStack => UnarySpec
specImagPart =
  UnarySpec {toOp = ImagPart, decideShape = id, decideET = decideET}
  where
    decideET x
      | x == C = R
      | otherwise = error "Must be complex"

specConjugate :: HasCallStack => UnarySpec
specConjugate = defaultUnary Conjugate [C]

specInnerProd :: HasCallStack => BinarySpec
specInnerProd =
  BinarySpec
    { toOp = InnerProd,
      decideShape = decideShape,
      decideET = decideET
    }
  where
    decideShape x y = assertSame [x, y] []
    decideET x y = assertSame [x, y] x

specPiecewise :: HasCallStack => [Double] -> ConditionarySpec
specPiecewise marks =
  ConditionarySpec {toOp = Piecewise marks, decideShape = decideShape, decideET = decideET}
  where
    decideShape condition branches = assertSame (condition : branches) condition
    decideET condition branches
      | condition == R && length branches == length marks + 1 = head branches
      | otherwise = error "Condition must be real and number of branches must equal number of marks + 1"

specRotate :: HasCallStack => RotateAmount -> UnarySpec
specRotate ra = defaultUnary (Rotate ra) [R, C]

specFT :: HasCallStack => UnarySpec
specFT = defaultUnary FT [C]

specIFT :: HasCallStack => UnarySpec
specIFT = defaultUnary IFT [C]

-------------------------------------------------------------------------------
processDimSelector :: Shape -> [DimSelector] -> Shape
processDimSelector [] [] = []
processDimSelector (size : xs) ((Range start end step) : ss) = (((end - start) `mod` size) `div` step + 1) : processDimSelector xs ss
processDimSelector (_ : xs) ((At _) : ss) = processDimSelector xs ss -- collapse the corresponding dimension

specProject :: HasCallStack => [DimSelector] -> UnarySpec
specProject dmSelectors =
  UnarySpec {toOp = Project dmSelectors, decideShape = decideShape, decideET = id}
  where
    decideShape shape
      | length shape == length dmSelectors = processDimSelector shape dmSelectors
      | otherwise = error "dim selectors and shape must be of same length"

specInject :: HasCallStack => [DimSelector] -> BinarySpec
specInject dmSelectors =
  BinarySpec {toOp = Inject dmSelectors, decideShape = decideShape, decideET = decideET}
  where
    decideShape subShape baseShape
      | length baseShape == length dmSelectors,
        subShape == processDimSelector baseShape dmSelectors =
        baseShape
      | otherwise = error $ "dim selectors, sub shape and base shape not valid" ++ show dmSelectors ++ " " ++ show subShape ++ " " ++ show baseShape
    decideET x y = assertSame [x, y] x

-------------------------------------------------------------------------------
specMatMul :: HasCallStack => BinarySpec
specMatMul = BinarySpec {toOp = MatMul, decideShape = decideShape, decideET = decideET}
  where
    decideShape [m, n] [p, q]
      | n == p = [m, q]
      | otherwise = error $ "invalid shape matrix multiplication" ++ show [m, n] ++ " " ++ show [p, q]
    decideShape [m, n] [p]
      | n == p = [m]
      | otherwise = error $ "invalid shape matrix multiplication" ++ show [m, n] ++ " " ++ show [p]
    decideShape shape1 shape2 = error $ "invalid shape matrix multiplication" ++ show shape1 ++ " " ++ show shape2
    decideET x y = assertSame [x, y] x

specTranspose :: HasCallStack => UnarySpec
specTranspose = UnarySpec {toOp = Transpose, decideShape = decideShape, decideET = id}
  where
    decideShape [m, n] = [n, m]
    -- decideShape [m] = [1, m]
    decideShape _ = error "invalid shape tranpose"

specCoerce :: HasCallStack => Shape -> UnarySpec
specCoerce targetShape = UnarySpec {toOp = Coerce targetShape, decideShape = decideShape, decideET = id}
  where
    decideShape shape
      | coercible shape targetShape || coercible targetShape shape = targetShape
    decideShape _ = error "not coercible"
