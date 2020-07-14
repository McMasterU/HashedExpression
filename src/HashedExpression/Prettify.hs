-- |
-- Module      :  HashedExpression.Prettify
-- Copyright   :  (c) OCA 2020
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  anandc@mcmaster.ca
-- Stability   :  provisional
-- Portability :  unportable
--
-- This module contains functionality for printing an 'Expression' in a readable (i.e pretty) format. This can be very
-- useful for debugging as the pretty format is still a syntactically valid 'Expression' that can be pasted into ghci
module HashedExpression.Prettify
  ( prettify,
    prettifyDebug,
    showExpDebug,
    showExp,
    showAllEntries,
    allEntriesDebug,
    allEntries,
    debugPrint,
  )
where

import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Typeable
import HashedExpression.Internal.Expression
import HashedExpression.Internal.Node

unwrap :: Expression d et -> (ExpressionMap, NodeID)
unwrap (Expression n mp) = (mp, n)

wrap :: (ExpressionMap, NodeID) -> Expression d et
wrap = uncurry $ flip Expression

-- | Automatically print a prettified expression (using 'prettify') to stdout.
--   If you wish to If you wish to enter the resulting pretty expression back into ghci, use 'showExpDebug'
showExp ::
  forall d rc.
  (Typeable d, Typeable rc) =>
  Expression d rc ->
  IO ()
showExp = putStrLn . prettify

-- | Visualize an 'Expression' in a pretty format. If you wish to enter the result into ghci, use 'prettifyDebug'
prettify ::
  forall d rc.
  (Typeable d, Typeable rc) =>
  Expression d rc ->
  String
prettify e@(Expression n mp) =
  let shape = expressionShape e
      node = expressionOp e
      dimensionStr
        | null shape = ""
        | otherwise = "(" ++ intercalate ", " (map show shape) ++ ") "
      typeName = " :: " ++ dimensionStr ++ (show . typeRep $ (Proxy :: Proxy rc))
   in T.unpack (hiddenPrettify False $ unwrap e) ++ typeName

-- | Automatically print a prettified expression (using 'prettify') to stdout. Generally, you can enter the result into
--   ghci as long as you define corresponding variable identifiers
showExpDebug :: forall d rc. (Typeable d, Typeable rc) => Expression d rc -> IO ()
showExpDebug = putStrLn . prettifyDebug

-- | Visualize an 'Expression' in a pretty format. Generally, you can re-enter a pretty printed 'Expression' into
--   ghci as long as you define corresponding variable identifiers
prettifyDebug :: Expression d rc -> String
prettifyDebug e@(Expression n mp) =
  let shape = expressionShape e
      node = expressionOp e
   in T.unpack (hiddenPrettify True $ unwrap e)

-- | All the entries of the expression
allEntries :: forall d rc. Expression d rc -> [(NodeID, String)]
allEntries (Expression n mp) =
  zip (IM.keys mp) . map (T.unpack . hiddenPrettify False . (mp,)) $
    IM.keys mp

-- | Print every entry (invididually) of an 'Expression', in a format that (in general) you should be able to enter into ghci
allEntriesDebug :: (ExpressionMap, NodeID) -> [(NodeID, String)]
allEntriesDebug (mp, n) =
  zip (IM.keys mp) . map (T.unpack . hiddenPrettify False . (mp,)) $
    IM.keys mp

-- | Print every entry (invididually) of an 'Expression'
showAllEntries :: forall d rc. Expression d rc -> IO ()
showAllEntries e = do
  putStrLn "--------------------------"
  putStrLn $ intercalate "\n" . map mkString $ allEntries e
  putStrLn "--------------------------"
  where
    mkString (n, str) = show n ++ " --> " ++ str

-- | same as 'prettify' without any overhead
debugPrint :: (ExpressionMap, NodeID) -> String
debugPrint = T.unpack . hiddenPrettify False

-- | auxillary function for computing pretty format of an 'Expression'
hiddenPrettify ::
  -- | retain syntactically valid (for use in ghci)
  Bool ->
  -- | (unwrapped) expression to be prettified
  (ExpressionMap, NodeID) ->
  -- | resulting "pretty" expression
  T.Text
hiddenPrettify pastable (mp, n) =
  let shape = retrieveShape n mp
      wrapParentheses x = T.concat ["(", x, ")"]
      node = retrieveOp n mp
      innerPrettify = hiddenPrettify pastable . (mp,)
      shapeSignature
        | pastable = ""
        | otherwise =
          case shape of
            [] -> ""
            [x] -> T.concat ["[", T.pack . show $ x, "]"]
            [x, y] -> T.concat ["[", T.pack . show $ x, "]", "[", T.pack . show $ y, "]"]
            [x, y, z] -> T.concat ["[", T.pack . show $ x, "]", "[", T.pack . show $ y, "]", "[", T.pack . show $ z, "]"]
            _ -> error "TODO: not yet support more than 3D"
   in case node of
        Var name -> T.concat [T.pack name]
        Const val
          | pastable ->
            case shape of
              [] -> T.concat ["const ", wrapParentheses . T.pack . show $ val]
              [x] -> T.concat ["const1d ", T.pack . show $ x, " ", wrapParentheses . T.pack . show $ val]
              _ -> T.pack $ show val
          | otherwise -> T.concat [T.pack . show $ val, shapeSignature]
        DVar name -> T.concat ["d", T.pack name]
        DZero -> "d0"
        _ ->
          wrapParentheses $
            case node of
              Sum args
                | pastable -> T.concat ["sum [", T.intercalate ", " . map innerPrettify $ args, "]"]
                | otherwise -> T.intercalate "+" . map innerPrettify $ args
              Mul args
                | pastable -> T.concat ["prod [", T.intercalate ", " . map innerPrettify $ args, "]"]
                | otherwise -> T.intercalate "*" . map innerPrettify $ args
              Neg arg
                | pastable -> T.concat ["negate", wrapParentheses $ innerPrettify arg]
                | otherwise -> T.concat ["-", wrapParentheses $ innerPrettify arg]
              Scale arg1 arg2 -> T.concat [innerPrettify arg1, "*.", innerPrettify arg2]
              Div arg1 arg2 -> T.concat [innerPrettify arg1, "/", innerPrettify arg2]
              Sqrt arg -> T.concat ["sqrt", wrapParentheses $ innerPrettify arg]
              Sin arg -> T.concat ["sin", wrapParentheses $ innerPrettify arg]
              Cos arg -> T.concat ["cos", wrapParentheses $ innerPrettify arg]
              Tan arg -> T.concat ["tan", wrapParentheses $ innerPrettify arg]
              Exp arg -> T.concat ["exp", wrapParentheses $ innerPrettify arg]
              Log arg -> T.concat ["log", wrapParentheses $ innerPrettify arg]
              Sinh arg -> T.concat ["sinh", wrapParentheses $ innerPrettify arg]
              Cosh arg -> T.concat ["cosh", wrapParentheses $ innerPrettify arg]
              Tanh arg -> T.concat ["tanh", wrapParentheses $ innerPrettify arg]
              Asin arg -> T.concat ["asin", wrapParentheses $ innerPrettify arg]
              Acos arg -> T.concat ["acos", wrapParentheses $ innerPrettify arg]
              Atan arg -> T.concat ["atan", wrapParentheses $ innerPrettify arg]
              Asinh arg -> T.concat ["asinh", wrapParentheses $ innerPrettify arg]
              Acosh arg -> T.concat ["acosh", wrapParentheses $ innerPrettify arg]
              Atanh arg -> T.concat ["atanh", wrapParentheses $ innerPrettify arg]
              RealImag arg1 arg2 -> T.concat [innerPrettify arg1, "+:", innerPrettify arg2]
              RealPart arg -> T.concat ["Re", wrapParentheses $ innerPrettify arg]
              ImagPart arg -> T.concat ["Im", wrapParentheses $ innerPrettify arg]
              InnerProd arg1 arg2 -> T.concat [innerPrettify arg1, "<.>", innerPrettify arg2]
              Piecewise marks conditionArg branches ->
                let printBranches = T.intercalate ", " . map innerPrettify $ branches
                 in T.concat ["piecewise ", T.pack . show $ marks, " ", innerPrettify conditionArg, " [", printBranches, "]"]
              Rotate amount arg -> T.concat ["rotate", T.pack . show $ amount, innerPrettify arg]
              Power x arg -> T.concat [innerPrettify arg, "^", T.pack $ show x]
              ReFT arg -> T.concat ["reFT", wrapParentheses $ innerPrettify arg]
              ImFT arg -> T.concat ["imFT", wrapParentheses $ innerPrettify arg]
              TwiceReFT arg -> T.concat ["twiceReFT", wrapParentheses $ innerPrettify arg]
              TwiceImFT arg -> T.concat ["twiceImFT", wrapParentheses $ innerPrettify arg]
              MulD arg1 arg2 -> T.concat [innerPrettify arg1, "*", innerPrettify arg2]
              ScaleD arg1 arg2 -> T.concat [innerPrettify arg1, "*.", innerPrettify arg2]
              DScale arg1 arg2 -> T.concat [innerPrettify arg1, "*.", innerPrettify arg2]
              InnerProdD arg1 arg2 -> T.concat [innerPrettify arg1, "<.>", innerPrettify arg2]

