module HashedExpression.Codegen.CSimple where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', partition)
import qualified Data.Set as Set
import HashedExpression.Codegen
import HashedExpression.Internal.Expression (ET (..), ExpressionMap)
import HashedExpression.Internal.Node (nodeElementType, retrieveInternal)
import HashedExpression.Internal.Utils (ValMaps)

data CSimpleConfig
  = CSimpleConfig
      { consecutiveIDs :: [Int]
      }

type Address = Int

data CSimpleCodegen
  = CSimpleCodegen
      { expressionMap :: ExpressionMap,
        address :: Int -> Address,
        memSize :: Int
      }

instance Codegen CSimpleCodegen CSimpleConfig where
  initCodegen :: ExpressionMap -> CSimpleConfig -> CSimpleCodegen
  initCodegen mp config =
    CSimpleCodegen
      { expressionMap = mp,
        address = addressMap,
        memSize = totalSize
      }
    where
      (cs, rest) = partition (`Set.member` Set.fromList (consecutiveIDs config)) (IM.keys mp)
      f (addressMap, curSize) nID =
        let (shape, node) = retrieveInternal nID mp
            et = nodeElementType node mp
         in case et of
              R -> (IM.insert nID curSize addressMap, curSize + product shape)
              C -> (IM.insert nID curSize addressMap, curSize + 2 * product shape)
      (memMap, totalSize) = foldl' f (IM.empty, 0) $ cs ++ rest
      addressMap nID
        | Just offset <- IM.lookup nID memMap = offset
        | otherwise = error "Node ID not exists in address map"

  -- generate code for assigning values provided with given ValMaps
  assigningValues :: CSimpleCodegen -> ValMaps -> Code
  assigningValues = undefined

  -- Generate code for evaluating target node ids
  evaluating :: CSimpleCodegen -> [Int] -> Code
  evaluating = undefined
