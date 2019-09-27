{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HashedPrettify
    ( prettify
    , prettifyDebug
    , showExpDebug
    , showExp
    , showAllEntries
    , allEntriesDebug
    , allEntries
    , debugPrint
    ) where

import qualified Data.IntMap.Strict as IM
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Typeable
import HashedExpression
import HashedNode

-- |
--
unwrap :: Expression d et -> (ExpressionMap, Int)
unwrap (Expression n mp) = (mp, n)

wrap :: (ExpressionMap, Int) -> Expression d et
wrap = uncurry $ flip Expression

-- | Pretty exp
--
showExp ::
       forall d rc. (Typeable d, Typeable rc)
    => Expression d rc
    -> IO ()
showExp = putStrLn . prettify

prettify ::
       forall d rc. (Typeable d, Typeable rc)
    => Expression d rc
    -> String
prettify e@(Expression n mp) =
    let shape = expressionShape e
        node = expressionNode e
        dimensionStr
            | null shape = ""
            | otherwise = "(" ++ intercalate ", " (map show shape) ++ ")"
        typeName =
            " :: " ++
            dimensionStr ++ " " ++ (show . typeRep $ (Proxy :: Proxy rc))
     in T.unpack (hiddenPrettify False $ unwrap e) ++ typeName

-- | Pretty exp to a string that can be paste to editor
--
showExpDebug ::
       forall d rc. (Typeable d, Typeable rc)
    => Expression d rc
    -> IO ()
showExpDebug = putStrLn . prettifyDebug

prettifyDebug :: Expression d rc -> String
prettifyDebug e@(Expression n mp) =
    let shape = expressionShape e
        node = expressionNode e
     in T.unpack (hiddenPrettify True $ unwrap e)

-- | All the entries of the expression
--
allEntries :: forall d rc. Expression d rc -> [(Int, String)]
allEntries (Expression n mp) =
    zip (IM.keys mp) . map (T.unpack . hiddenPrettify False . (mp, )) $
    IM.keys mp

allEntriesDebug :: (ExpressionMap, Int) -> [(Int, String)]
allEntriesDebug (mp, n) =
    zip (IM.keys mp) . map (T.unpack . hiddenPrettify False . (mp, )) $
    IM.keys mp

-- |
--
showAllEntries :: forall d rc. Expression d rc -> IO ()
showAllEntries e = do
    putStrLn "--------------------------"
    putStrLn $ intercalate "\n" . map mkString $ allEntries e
    putStrLn "--------------------------"
  where
    mkString (n, str) = show n ++ " --> " ++ str

-- |
--
debugPrint :: (ExpressionMap, Int) -> String
debugPrint = T.unpack . hiddenPrettify False

-- | 
--
hiddenPrettify :: Bool -> (ExpressionMap, Int) -> T.Text
hiddenPrettify pastable (mp, n) =
    let shape = retrieveShape n mp
        wrapParentheses x = T.concat ["(", x, ")"]
        node = retrieveNode n mp
        innerPrettify = hiddenPrettify pastable . (mp, )
        shapeSignature
            | pastable = ""
            | otherwise =
                case shape of
                    [] -> ""
                    [x] -> T.concat ["[", T.pack . show $ x, "]"]
                    [x, y] ->
                        T.concat
                            [ "["
                            , T.pack . show $ x
                            , "]"
                            , "["
                            , T.pack . show $ y
                            , "]"
                            ]
                    [x, y, z] ->
                        T.concat
                            [ "["
                            , T.pack . show $ x
                            , "]"
                            , "["
                            , T.pack . show $ y
                            , "]"
                            , "["
                            , T.pack . show $ z
                            , "]"
                            ]
                    _ -> error "Haven't deal with more than 3-dimension"
     in case node of
            Var name -> T.concat [T.pack name]
            DVar name -> T.concat ["d", T.pack name]
            Const val
                | pastable ->
                    case shape of
                        [] ->
                            T.concat
                                [ "const "
                                , wrapParentheses . T.pack . show $ val
                                ]
                        [x] ->
                            T.concat
                                [ "const1d "
                                , T.pack . show $ x
                                , " "
                                , wrapParentheses . T.pack . show $ val
                                ]
                | otherwise -> T.concat [T.pack . show $ val, shapeSignature]
            _ ->
                wrapParentheses $
                case node of
                    Sum _ args
                        | pastable ->
                            T.concat
                                [ "sum ["
                                , T.intercalate ", " . map innerPrettify $ args
                                , "]"
                                ]
                        | otherwise ->
                            T.intercalate "+" . map innerPrettify $ args
                    Mul _ args
                        | pastable ->
                            T.concat
                                [ "prod ["
                                , T.intercalate ", " . map innerPrettify $ args
                                , "]"
                                ]
                        | otherwise ->
                            T.intercalate "*" . map innerPrettify $ args
                    Neg _ arg
                        | pastable ->
                            T.concat
                                ["negate", wrapParentheses $ innerPrettify arg]
                        | otherwise ->
                            T.concat ["-", wrapParentheses $ innerPrettify arg]
                    Scale _ arg1 arg2 ->
                        T.concat [innerPrettify arg1, "*.", innerPrettify arg2]
                    Div arg1 arg2 ->
                        T.concat [innerPrettify arg1, "/", innerPrettify arg2]
                    Sqrt arg ->
                        T.concat ["sqrt", wrapParentheses $ innerPrettify arg]
                    Sin arg ->
                        T.concat ["sin", wrapParentheses $ innerPrettify arg]
                    Cos arg ->
                        T.concat ["cos", wrapParentheses $ innerPrettify arg]
                    Tan arg ->
                        T.concat ["tan", wrapParentheses $ innerPrettify arg]
                    Exp arg ->
                        T.concat ["exp", wrapParentheses $ innerPrettify arg]
                    Log arg ->
                        T.concat ["log", wrapParentheses $ innerPrettify arg]
                    Sinh arg ->
                        T.concat ["sinh", wrapParentheses $ innerPrettify arg]
                    Cosh arg ->
                        T.concat ["cosh", wrapParentheses $ innerPrettify arg]
                    Tanh arg ->
                        T.concat ["tanh", wrapParentheses $ innerPrettify arg]
                    Asin arg ->
                        T.concat ["asin", wrapParentheses $ innerPrettify arg]
                    Acos arg ->
                        T.concat ["acos", wrapParentheses $ innerPrettify arg]
                    Atan arg ->
                        T.concat ["atan", wrapParentheses $ innerPrettify arg]
                    Asinh arg ->
                        T.concat ["asinh", wrapParentheses $ innerPrettify arg]
                    Acosh arg ->
                        T.concat ["acosh", wrapParentheses $ innerPrettify arg]
                    Atanh arg ->
                        T.concat ["atanh", wrapParentheses $ innerPrettify arg]
                    RealImag arg1 arg2 ->
                        T.concat [innerPrettify arg1, "+:", innerPrettify arg2]
                    RealPart arg ->
                        T.concat ["Re", wrapParentheses $ innerPrettify arg]
                    ImagPart arg ->
                        T.concat ["Im", wrapParentheses $ innerPrettify arg]
                    InnerProd et arg1 arg2 ->
                        T.concat [innerPrettify arg1, "<.>", innerPrettify arg2]
                    Piecewise marks conditionArg branches ->
                        let printBranches =
                                T.intercalate ", " . map innerPrettify $
                                branches
                         in T.concat
                                [ "piecewise "
                                , T.pack . show $ marks
                                , " "
                                , innerPrettify conditionArg
                                , " ["
                                , printBranches
                                , "]"
                                ]
                    Rotate amount arg ->
                        T.concat
                            [ "rotate"
                            , T.pack . show $ amount
                            , innerPrettify arg
                            ]
                    Power x arg ->
                        T.concat [innerPrettify arg, "^", T.pack $ show x]
                    ReFT arg ->
                        T.concat ["reFT", wrapParentheses $ innerPrettify arg]
                    ImFT arg ->
                        T.concat ["imFT", wrapParentheses $ innerPrettify arg]
                    TwiceReFT arg ->
                        T.concat
                            ["twiceReFT", wrapParentheses $ innerPrettify arg]
                    TwiceImFT arg ->
                        T.concat
                            ["twiceImFT", wrapParentheses $ innerPrettify arg]
