{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HashedPrettify
    ( prettify
    ) where

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Typeable
import HashedExpression

prettify ::
       forall d rc. (Typeable d, Typeable rc)
    => Expression d rc
    -> String
prettify e@(Expression n mp) =
    let shape = expressionShape e
        node = expressionNode e
        typeName =
            " :: " ++
            (show . typeRep $ (Proxy :: Proxy d)) ++
            " " ++ (show . typeRep $ (Proxy :: Proxy rc))
     in T.unpack (hiddenPrettify e) ++ typeName

hiddenPrettify :: Expression d rc -> T.Text
hiddenPrettify e@(Expression n mp) =
    let shape = expressionShape e
        wrapParentheses x = T.concat ["(", x, ")"]
        node = expressionNode e
        shapeSignature =
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
     in case node of
            Var name -> T.concat [T.pack name, shapeSignature]
            DVar name -> T.concat ["d", T.pack name, shapeSignature]
            Const val -> T.concat [T.pack . show $ val, shapeSignature]
            Sum _ args ->
                wrapParentheses .
                T.intercalate "+" . map (hiddenPrettify . flip Expression mp) $
                args
            Mul _ args ->
                wrapParentheses .
                T.intercalate "*" . map (hiddenPrettify . flip Expression mp) $
                args
            Sin arg -> T.concat ["sin(", hiddenPrettify . flip Expression mp $ arg, ")"]
            Cos arg -> T.concat ["cos(", hiddenPrettify . flip Expression mp $ arg, ")"]
