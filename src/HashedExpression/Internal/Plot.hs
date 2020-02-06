module HashedExpression.Internal.Plot where

-------------------------------------------------------------------------------
-- | Require: gnuplot (http://www.gnuplot.info/)
--
--
-------------------------------------------------------------------------------
import Graphics.EasyPlot

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import HashedExpression.Internal.Expression (Expression, Node(..), R, Scalar, exMap)
import HashedExpression.Interp
import HashedExpression.Internal.Utils
import System.Process (readProcessWithExitCode)

type FileName = String

data Function =
    Function
        { expr :: Expression Scalar R
        , predefinedValues :: ValMaps
        }

scalarVariables :: Function -> [String]
scalarVariables (Function exp values) = mapMaybe toScalarVariable entries
  where
    entries = IM.elems . exMap $ exp
    toScalarVariable (shape, node)
        | Var name <- node
        , null shape
        , not $ Map.member name values = Just name
        | otherwise = Nothing

plot1VariableFunction :: Function -> FileName -> IO ()
plot1VariableFunction fn@(Function exp values) imageName
    | [var] <- scalarVariables fn =
        let f x = eval (Map.insert var (VScalar x) values) exp
         in do plot (PDF $ "plots/" ++ imageName ++ ".pdf") $
                   Function2D [Title "Function"] [] f
               readProcessWithExitCode "rm" ["plot1.dat"] ""
               putStrLn "Done plotting !"
               putStrLn $
                   "Your image " ++ imageName ++ ".pdf is in plots folder"
    | otherwise = do
        putStrLn
            "Expression and values provided together is not a 1-variable function !!"
        putStrLn "Can't do plotting"
