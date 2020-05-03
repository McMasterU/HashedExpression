module HashedExpression.Codegen where

import Data.Text (Text)
import qualified Data.Text as T
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Utils (ValMaps)

-- | List where each element is a line of code
type Code = [Text]

data CodegenInit
  = CodegenInit
      { expressionMap :: ExpressionMap,
        consecutiveIDs :: [Int]
        -- more common options here
      }

-- | The typeclass for code generating
class Codegen codegen configs | configs -> codegen, codegen -> configs where
  -- Init all necessary infos and data for later code gen
  initCodegen :: CodegenInit -> configs -> codegen

  -- generate code for assigning values provided with given ValMaps
  assigningValues :: codegen -> ValMaps -> Code

  -- Generate code for evaluating target node ids
  evaluating :: codegen -> [Int] -> Code

-- | Indent `n` space each line of code
indent :: Int -> Code -> Code
indent n = map (T.pack (replicate n ' ') <>)
