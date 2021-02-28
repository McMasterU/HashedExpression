{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import qualified Data.IntMap.Strict as IM
import qualified Data.Text.Lazy as LT
import Examples.Brain
import Examples.LinearRegression
import Examples.LogisticRegression
import Examples.NeuralNetwork
import HashedExpression
import HashedExpression.Modeling.Typed
import HashedExpression.Prettify
import Prelude hiding ((^))

f =
  let y = variable "y"
      x = variable2D @10 @10 "x"
      z = y *. x
      t = y *. x
   in z + t

main :: IO ()
main = do
  -- let x = variable2D @128 @128 "x"
  --     --- bound
  --     xLowerBound = bound2D @128 @128 "x_lb"
  --     xUpperBound = bound2D @128 @128 "x_ub"
  --     -- parameters
  --     im = param2D @128 @128 "im"
  --     re = param2D @128 @128 "re"
  --     mask = param2D @128 @128 "mask"
  --     -- regularization
  --     regularization = norm2square (rotate (0, 1) x - x) + norm2square (rotate (1, 0) x - x)
  --     lambda = 3000
  --     objective =
  --           norm2square ((mask +: 0) * (ft (x +: 0) - (re +: im)))
  --             + lambda * regularization
  let y = variable "y"
      x = variable2D @10 @10 "x"
      z = y *. x
      t = y *. x
      f = z + t
  print f

-- let gr1 = fst $ asRawExpr objective
-- let gr2 = fst $ asRawExpr z
-- print $ IM.union gr1 gr2
-- let a =
--       fromList
--         [ (41509096, ([], R, Var "x")),
--           (42080883, ([], R, Var "y")),
--           (761793024715737, ([], R, Const 2.0)),
--           (989450128496424, ([], R, Sum [41509096, 42080883])),
--           (1166617864868246, ([], R, Mul [761793024715737, 989450128496424])),
--           (1297662858261074, ([], R, Power 2 989450128496424))
--         ]
-- writeFile "haha.dot" $ toDotCode ShowFullExpr $ IM.union gr1 gr2

-- case constructProblem objective [] of
--   Right problem -> writeFile "haha.dot" $ toDotCodeProblem ShowFullExpr problem
-- let x = variable "x"
-- let y = variable "y"

-- let exp = x * (x + y) * log (x + y)
-- -- print exp
-- writeFile "haha.dot" $ toDotCode ShowFullExpr $ x * (x + y) * log (x + y)
