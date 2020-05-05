module HashedExpression.Codegen where

import Data.Text (Text)
import qualified Data.Text as T
import HashedExpression.Internal.Expression
import HashedExpression.Problem
import HashedExpression.Value

-- | List where each element is a line of code
type Code = [Text]

-- |
data GenResult
  = Invalid String
  | -- Write all the necessary files in a given file path
    Success (String -> IO ())

data CodegenInit
  = CodegenInit
      { codegenExMap :: ExpressionMap,
        codegenConsecutiveIDs :: [Int]
        -- more common options here
      }

-- | The type class for code generating
class Codegen configs where
  generateProblemCode :: configs -> Problem -> ValMaps -> GenResult

-- | Indent `n` space each line of code
indent :: Int -> Code -> Code
indent n = map (T.pack (replicate n ' ') <>)
