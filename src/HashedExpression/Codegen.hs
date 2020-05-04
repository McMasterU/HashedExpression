module HashedExpression.Codegen where

import Data.Text (Text)
import qualified Data.Text as T
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Utils (ValMaps)
import HashedExpression.Problem

-- | List where each element is a line of code
type Code = [Text]

-- |
data GenResult
  = Invalid String
  | -- Give it the name of the folder of your solver, it will write all the files necessary
    Success (String -> IO ())

data CodegenInit
  = CodegenInit
      { codegenExMap :: ExpressionMap,
        codegenConsecutiveIDs :: [Int]
        -- more common options here
      }

-- | The typeclass for code generating
class Codegen codegen configs | configs -> codegen, codegen -> configs where
  -- Init all necessary infos and data for later code gen
  initCodegen :: CodegenInit -> configs -> codegen
  -- Generate code for evaluating target node ids
  evaluating :: codegen -> [Int] -> Code
  -- Generate problem code 
  generateProblemCode :: configs -> Problem -> ValMaps -> GenResult

-- | Indent `n` space each line of code
indent :: Int -> Code -> Code
indent n = map (T.pack (replicate n ' ') <>)
