-- |
-- Module      :  HashedExpression.Prettify
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module contains functionality for printing an 'TypedExpr' in a readable (i.e pretty) format. This can be very
-- useful for debugging as the pretty format is still a syntactically valid 'TypedExpr' that can be pasted into ghci
module HashedExpression.Prettify where

import Data.Function ((&))
import qualified Data.GraphViz as GV
import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import HashedExpression.Internal.Base
import HashedExpression.Internal.Node
import HashedExpression.Problem
import HashedExpression.Utils (ToText (tt))

--------------------------------------------------------------------------------
instance GV.PrintDot NodeID where
  unqtDot (NodeID nID) = GV.unqtDot nID

--------------------------------------------------------------------------------
class HasExpressionMap t where
  getExpressionMap :: t -> ExpressionMap

instance IsExpression e => HasExpressionMap e where
  getExpressionMap = fst . asRawExpr

instance HasExpressionMap ExpressionMap where
  getExpressionMap = id

instance HasExpressionMap Problem where
  getExpressionMap = expressionMap

--------------------------------------------------------------------------------

data PrintMode = PrintText | PrintLatex

data DotGraphMode = ShowOp | ShowFullExpr | ShowFullExprWithID

--------------------------------------------------------------------------------

textLabel :: T.Text -> GV.Attribute
textLabel = GV.textLabel . LT.pack . T.unpack

buildLine :: [T.Text] -> T.Text
buildLine xs = T.intercalate "\n" xs

inTextTag :: T.Text -> T.Text
inTextTag tx = "\\text{" <> tx <> "}"

toDotCode :: HasExpressionMap t => DotGraphMode -> t -> String
toDotCode dotGraphMode = LT.unpack . GV.printDotGraph . toDotGraph' dotGraphMode . getExpressionMap

toDotCodeProblem :: DotGraphMode -> Problem -> String
toDotCodeProblem mode problem =
  let stmts = GV.graphStatements $ toDotGraph' mode $ expressionMap problem
      fExtraID = NodeID (-1)
      partials =
        problem
          & variables
          & zip (map NodeID [-2, -3 ..])
          & map (\(extraID, Variable {..}) -> (extraID, varName, partialDerivativeId))

      dotGraph =
        GV.DotGraph
          False
          True
          Nothing
          GV.DotStmts
            { attrStmts = GV.attrStmts stmts,
              subGraphs = GV.subGraphs stmts,
              nodeStmts =
                GV.nodeStmts stmts
                  ++ [GV.DotNode fExtraID [GV.toLabel "f"]]
                  ++ map
                    ( \(extraID, varName, _) ->
                        GV.DotNode extraID [GV.toLabel $ "\\frac{\\partial{f}}{\\partial{" <> varName <> "}}"]
                    )
                    partials,
              edgeStmts =
                GV.edgeStmts stmts
                  ++ [GV.DotEdge (objectiveId problem) fExtraID []]
                  ++ map
                    ( \(extraID, _, partialDerivativeId) ->
                        GV.DotEdge partialDerivativeId extraID []
                    )
                    partials
            }
   in LT.unpack . GV.printDotGraph $ dotGraph

toDotGraph' :: DotGraphMode -> ExpressionMap -> GV.DotGraph NodeID
toDotGraph' dotGraphMode mp =
  GV.DotGraph
    { strictGraph = False,
      directedGraph = True,
      graphID = Nothing,
      graphStatements =
        GV.DotStmts
          { attrStmts = [],
            subGraphs = [],
            nodeStmts =
              mp
                & IM.toList
                & map
                  ( \(nID, (_, _, op)) -> case dotGraphMode of
                      ShowFullExpr -> GV.DotNode (NodeID nID) [GV.toLabel $ prettify' PrintLatex mp (NodeID nID)]
                      ShowFullExprWithID ->
                        GV.DotNode
                          (NodeID nID)
                          [ textLabel $
                              buildLine [prettify' PrintLatex mp (NodeID nID), inTextTag $ tt nID, inTextTag $ tt (show op)]
                          ]
                      ShowOp ->
                        GV.DotNode
                          (NodeID nID)
                          [ GV.toLabel $ case op of
                              Var name -> name
                              Param name -> name
                              Const val -> show val
                              Sum {} -> "Sum"
                              Mul {} -> "Mul"
                              Power x _ -> "Power " <> show x
                              Neg {} -> "Neg"
                              Scale {} -> "Scale"
                              Div {} -> "Div"
                              Sqrt {} -> "Sqrt"
                              Sin {} -> "Sin"
                              Cos {} -> "Cos"
                              Tan {} -> "Tan"
                              Exp {} -> "Exp"
                              Log {} -> "Log"
                              Sinh {} -> "Sinh"
                              Cosh {} -> "Cosh"
                              Tanh {} -> "Tanh"
                              Asin {} -> "Asin"
                              Acos {} -> "Acos"
                              Atan {} -> "Atan"
                              Asinh {} -> "Asinh"
                              Acosh {} -> "Acosh"
                              Atanh {} -> "Atanh"
                              RealImag {} -> "RealImag"
                              RealPart {} -> "RealPart"
                              ImagPart {} -> "ImagPart"
                              Conjugate {} -> "Conjugate"
                              InnerProd {} -> "InnerProd"
                              Piecewise {} -> "Piecewise"
                              Rotate {} -> "Rotate"
                              FT {} -> "FT"
                              IFT {} -> "IFT"
                              Project {} -> "Project"
                              Inject {} -> "Inject"
                              MatMul {} -> "MatMul"
                              Transpose {} -> "Transpose"
                              Coerce {} -> "Coerce"
                          ]
                  ),
            edgeStmts =
              mp
                & IM.toList
                & concatMap
                  ( \(nID, (_, _, op)) ->
                      opArgs op
                        & map (\argID -> GV.DotEdge argID (NodeID nID) [])
                  )
          }
    }

-- | Automatically print a prettified expression (using 'prettify') to stdout.
showExp :: IsExpression e => e -> IO ()
showExp = putStrLn . prettify

-- |
prettify :: IsExpression e => e -> String
prettify e =
  let (mp, n) = asRawExpr e
      (shape, et, _) = retrieveNode n mp
      dimensionStr
        | null shape = ""
        | otherwise = "(" ++ intercalate ", " (map show shape) ++ ") "
      typeName = " :: " ++ dimensionStr ++ show et
   in T.unpack (prettify' PrintText mp n) ++ typeName

-- |
prettifyDimSelector :: DimSelector -> String
prettifyDimSelector (At i) = show i
prettifyDimSelector (Range start end 1) = show start ++ ":" ++ show end
prettifyDimSelector (Range start end n) = show start ++ ":" ++ show end ++ ":" ++ show n

-- |
prettifyAll :: PrintMode -> ExpressionMap -> [(NodeID, String)]
prettifyAll printMode mp =
  mp
    & IM.keys
    & map NodeID
    & map (\nID -> (nID, T.unpack $ prettify' printMode mp nID))

-- | auxiliary function for computing pretty format of an 'TypedExpr'
prettify' :: PrintMode -> ExpressionMap -> NodeID -> T.Text
prettify' printMode mp n =
  let wrapParentheses x = T.concat ["(", x, ")"]
      innerPrettify = wrapParentheses . prettify' printMode mp
      innerPrettifyNoParen = prettify' printMode mp
   in case retrieveOp n mp of
        Var name -> T.pack name
        Param name -> T.pack name
        Const val -> T.pack . show $ val
        Coerce _ arg -> innerPrettifyNoParen arg
        Sum args -> T.intercalate "+" . map innerPrettifyNoParen $ args
        Mul args -> T.intercalate "*" . map innerPrettify $ args
        Neg arg -> T.concat ["-", innerPrettify arg]
        Scale arg1 arg2 -> T.concat [innerPrettify arg1, "*.", innerPrettify arg2]
        Div arg1 arg2 -> T.concat [innerPrettify arg1, "/", innerPrettify arg2]
        Sqrt arg -> T.concat ["sqrt", innerPrettify arg]
        Sin arg -> T.concat ["sin", innerPrettify arg]
        Cos arg -> T.concat ["cos", innerPrettify arg]
        Tan arg -> T.concat ["tan", innerPrettify arg]
        Exp arg -> T.concat ["exp", innerPrettify arg]
        Log arg -> T.concat ["log", innerPrettify arg]
        Sinh arg -> T.concat ["sinh", innerPrettify arg]
        Cosh arg -> T.concat ["cosh", innerPrettify arg]
        Tanh arg -> T.concat ["tanh", innerPrettify arg]
        Asin arg -> T.concat ["asin", innerPrettify arg]
        Acos arg -> T.concat ["acos", innerPrettify arg]
        Atan arg -> T.concat ["atan", innerPrettify arg]
        Asinh arg -> T.concat ["asinh", innerPrettify arg]
        Acosh arg -> T.concat ["acosh", innerPrettify arg]
        Atanh arg -> T.concat ["atanh", innerPrettify arg]
        RealImag arg1 arg2 -> T.concat [innerPrettify arg1, "+", innerPrettify arg2, "*i"]
        RealPart arg -> T.concat ["Re", innerPrettify arg]
        ImagPart arg -> T.concat ["Im", innerPrettify arg]
        Conjugate arg -> T.concat ["conjugate", innerPrettify arg]
        InnerProd arg1 arg2 -> T.concat [innerPrettify arg1, "<.>", innerPrettify arg2]
        Piecewise marks conditionArg branches ->
          let printBranches = T.intercalate ", " . map innerPrettifyNoParen $ branches
           in T.concat ["piecewise ", T.pack . show $ marks, " ", innerPrettify conditionArg, " [", printBranches, "]"]
        Rotate amount arg -> T.concat ["rotate", T.pack . show $ amount, innerPrettify arg]
        Power x arg -> T.concat [innerPrettify arg, "^", T.pack $ show x]
        FT arg -> T.concat ["FT", innerPrettify arg]
        IFT arg -> T.concat ["IFT", innerPrettify arg]
        Project ss arg ->
          T.concat
            [ innerPrettify arg,
              "[",
              T.intercalate "," (map (T.pack . prettifyDimSelector) ss),
              "]"
            ]
        Inject ss sub base ->
          T.concat
            [ "inject(",
              innerPrettify sub,
              ", ",
              innerPrettify base,
              "[",
              T.intercalate "," (map (T.pack . prettifyDimSelector) ss),
              "]"
            ]
        MatMul arg1 arg2 -> T.concat [innerPrettify arg1, "**", innerPrettify arg2]
        Transpose arg -> T.concat ["transpose", innerPrettify arg]
