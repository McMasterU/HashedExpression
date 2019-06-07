{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HashedPrettify where

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
        typeName = " :: " ++ (show . typeRep $ (Proxy :: Proxy d)) ++ " " ++ (show . typeRep $ (Proxy :: Proxy rc))
     in T.unpack (hiddenPrettify e) ++ typeName

hiddenPrettify :: Expression d rc -> T.Text
hiddenPrettify e@(Expression n mp) =
    let shape = expressionShape e
        wrapParentheses x = T.concat ["(", x, ")"]
        node = expressionNode e
     in case node of
            Var name -> T.pack name
            DVar name -> T.concat ["d", T.pack name]
            Sum _ args ->
                wrapParentheses .
                T.intercalate "+" . map (hiddenPrettify . flip Expression mp) $
                args
            Mul _ args ->
                wrapParentheses .
                T.intercalate "*" . map (hiddenPrettify . flip Expression mp) $
                args

