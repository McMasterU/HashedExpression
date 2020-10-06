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
    asString,
    showExpDebug,
    showExp,
    hiddenPrettify,
    debugPrint,
    debugPrintExp,
  )
where

import Data.List (intercalate)
import qualified Data.Text as T
import HashedExpression.Internal.Base
import HashedExpression.Internal.Node

-- | Automatically print a prettified expression (using 'prettify') to stdout.
--   If you wish to If you wish to enter the resulting pretty expression back into ghci, use 'showExpDebug'
showExp :: IsExpression e => e -> IO ()
showExp = putStrLn . prettify

-- | Visualize an 'Expression' in a pretty format. If you wish to enter the result into ghci, use 'asString'
prettify :: IsExpression e => e -> String
prettify e =
  let (mp, n) = asRawExpr e
      (shape, et, _) = retrieveNode n mp
      dimensionStr
        | null shape = ""
        | otherwise = "(" ++ intercalate ", " (map show shape) ++ ") "
      typeName = " :: " ++ dimensionStr ++ show et
   in T.unpack (hiddenPrettify False $ asRawExpr e) ++ typeName

-- | Automatically print a prettified expression (using 'prettify') to stdout. Generally, you can enter the result into
--   ghci as long as you define corresponding variable identifiers
showExpDebug :: IsExpression e => e -> IO ()
showExpDebug = putStrLn . asString

-- | Visualize an 'Expression' in a pretty format. Generally, you can re-enter a pretty printed 'Expression' into
--   ghci as long as you define corresponding variable identifiers
asString :: IsExpression e => e -> String
asString e = T.unpack (hiddenPrettify True $ asRawExpr e)

-- |
prettifyDimSelector :: DimSelector -> String
prettifyDimSelector (At i) = show i
prettifyDimSelector (Range start end 1) = show start ++ ":" ++ show end
prettifyDimSelector (Range start end n) = show start ++ ":" ++ show end ++ ":" ++ show n

-- | same as 'prettify' without any overhead
debugPrint :: RawExpr -> String
debugPrint = T.unpack . hiddenPrettify False

debugPrintExp :: IsExpression e => e -> String
debugPrintExp = debugPrint . asRawExpr

-- | Print every entry (invididually) of an 'Expression'
-- showAllEntries :: forall d et. Expression d et -> IO ()
-- showAllEntries e = do
--  putStrLn "--------------------------"
--  putStrLn $ intercalate "\n" . map mkString $ allEntries e
--  putStrLn "--------------------------"
--  where
--    mkString (n, str) = show n ++ " --> " ++ str

-- | auxiliary function for computing pretty format of an 'Expression'
hiddenPrettify ::
  -- | retain syntactically valid (for use in ghci)
  Bool ->
  -- | (unwrapped) expression to be prettified
  RawExpr ->
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
        Var name -> T.pack name
        Param name -> T.pack name
        Const val
          | pastable ->
            case shape of
              [] -> T.concat ["const ", wrapParentheses . T.pack . show $ val]
              [x] -> T.concat ["const1d ", T.pack . show $ x, " ", wrapParentheses . T.pack . show $ val]
              _ -> T.pack $ show val
          | otherwise -> T.concat [T.pack . show $ val, shapeSignature]
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
              Conjugate arg -> T.concat ["conjugate", wrapParentheses $ innerPrettify arg]
              InnerProd arg1 arg2 -> T.concat [innerPrettify arg1, "<.>", innerPrettify arg2]
              Piecewise marks conditionArg branches ->
                let printBranches = T.intercalate ", " . map innerPrettify $ branches
                 in T.concat ["piecewise ", T.pack . show $ marks, " ", innerPrettify conditionArg, " [", printBranches, "]"]
              Rotate amount arg -> T.concat ["rotate", T.pack . show $ amount, innerPrettify arg]
              Power x arg -> T.concat [innerPrettify arg, "^", T.pack $ show x]
              FT arg -> T.concat ["FT", wrapParentheses $ innerPrettify arg]
              IFT arg -> T.concat ["IFT", wrapParentheses $ innerPrettify arg]
              Project ss arg ->
                T.concat
                  [ wrapParentheses $ innerPrettify arg,
                    "[",
                    T.intercalate "," (map (T.pack . prettifyDimSelector) ss),
                    "]"
                  ]
              Inject ss sub base ->
                T.concat
                  [ wrapParentheses $ innerPrettify sub,
                    " inject into ",
                    wrapParentheses $ innerPrettify base,
                    "[",
                    T.intercalate "," (map (T.pack . prettifyDimSelector) ss),
                    "]"
                  ]
              MatMul arg1 arg2 -> T.concat [innerPrettify arg1, "**", innerPrettify arg2]
              Transpose arg -> T.concat ["transpose", wrapParentheses $ innerPrettify arg]
