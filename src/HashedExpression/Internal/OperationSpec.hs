{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HashedExpression.Internal.OperationSpec where

import HashedExpression.Internal.Expression

data UnarySpec
  = UnarySpec
      { toOp :: Arg -> Op,
        decideShape :: Shape -> Shape,
        decideET :: ET -> ET
      }

data BinarySpec
  = BinarySpec
      { toOp :: Arg -> Arg -> Op,
        decideShape :: Shape -> Shape -> Shape,
        decideET :: ET -> ET -> ET
      }

data NarySpec
  = NarySpec
      { toOp :: [Arg] -> Op,
        decideShape :: [Shape] -> Shape,
        decideET :: [ET] -> ET
      }

data ConditionarySpec
  = ConditionarySpec
      { toOp :: Arg -> [Arg] -> Op,
        decideShape :: Shape -> [Shape] -> Shape,
        decideET :: ET -> [ET] -> ET
      }

data OperationSpec = Unary UnarySpec | Binary BinarySpec | Nary NarySpec | ConditionAry ConditionarySpec

specSum :: NarySpec
specSum = undefined

specMul :: NarySpec
specMul = undefined

specMulBinary :: BinarySpec
specMulBinary = undefined

specSumBinary :: BinarySpec
specSumBinary = undefined

specMulCovector :: BinarySpec
specMulCovector = undefined

specScaleCovector :: BinarySpec
specScaleCovector = undefined

specInnerProdCovector :: BinarySpec
specInnerProdCovector = undefined

specPower :: Int -> UnarySpec
specPower = undefined

specNeg :: UnarySpec
specNeg = undefined

specScale :: BinarySpec
specScale = undefined

specDiv :: BinarySpec
specDiv = undefined

specSqrt :: UnarySpec
specSqrt = undefined

specSin :: UnarySpec
specSin = undefined

specCos :: UnarySpec
specCos = undefined

specTan :: UnarySpec
specTan = undefined

specExp :: UnarySpec
specExp = undefined

specLog :: UnarySpec
specLog = undefined

specSinh :: UnarySpec
specSinh = undefined

specCosh :: UnarySpec
specCosh = undefined

specTanh :: UnarySpec
specTanh = undefined

specAsin :: UnarySpec
specAsin = undefined

specAcos :: UnarySpec
specAcos = undefined

specAtan :: UnarySpec
specAtan = undefined

specAsinh :: UnarySpec
specAsinh = undefined

specAcosh :: UnarySpec
specAcosh = undefined

specAtanh :: UnarySpec
specAtanh = undefined

specRealImag :: BinarySpec
specRealImag = undefined

specRealPart :: UnarySpec
specRealPart = undefined

specImagPart :: UnarySpec
specImagPart = undefined

specInnerProd :: BinarySpec
specInnerProd = undefined

specPiecewise :: [Double] -> ConditionarySpec
specPiecewise = undefined

specRotate :: RotateAmount -> UnarySpec
specRotate = undefined

specReFT :: UnarySpec
specReFT = undefined

specImFT :: UnarySpec
specImFT = undefined

specTwiceReFT :: UnarySpec
specTwiceReFT = undefined

specTwiceImFT :: UnarySpec
specTwiceImFT = undefined
