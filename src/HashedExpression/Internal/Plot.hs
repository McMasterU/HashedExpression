-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-- | Require: gnuplot (http://www.gnuplot.info/)
module HashedExpression.Internal.Plot where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Graphics.EasyPlot
import HashedExpression.Internal.Expression
  ( ElementType (..),
    Expression,
    Op (..),
    Scalar,
    exMap,
  )
import HashedExpression.Internal.Utils
import HashedExpression.Interp
import HashedExpression.Value
import System.Process (readProcessWithExitCode)

type FileName = String

data Function = Function
  { expr :: Expression Scalar R,
    predefinedValues :: ValMap
  }

scalarVariables :: Function -> [String]
scalarVariables (Function exp values) = mapMaybe toScalarVariable entries
  where
    entries = IM.elems . exMap $ exp
    toScalarVariable (shape, _, node)
      | Var name <- node,
        null shape,
        not $ Map.member name values =
        Just name
      | otherwise = Nothing

plot1VariableFunction :: Function -> FileName -> IO ()
plot1VariableFunction fn@(Function exp values) imageName
  | [var] <- scalarVariables fn = do
    let f x = let VR res = eval (Map.insert var (VScalar x) values) exp in res
    plot (PDF $ "plots/" ++ imageName ++ ".pdf") $
      Function2D [Title "Function"] [] f
    readProcessWithExitCode "rm" ["plot1.dat"] ""
    putStrLn "Done plotting !"
    putStrLn $ "Your image " ++ imageName ++ ".pdf is in plots folder"
  | otherwise = do
    putStrLn "Expression and values provided together is not a 1-variable function !!"
    putStrLn "Can't do plotting"
