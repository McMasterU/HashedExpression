{-# LANGUAGE ScopedTypeVariables #-}

module HashedPrettify where

import qualified Data.ByteString as B
import Data.List (intercalate)
import Data.Typeable
import HashedExpression

prettify :: (Typeable d, Typeable rc) => Expression d rc -> String
prettify e@(Expression n mp) =
    let shape = expressionShape e
        node = expressionNode e
        typeName = " :: " ++ (show . typeRep $ e)
     in hiddenPrettify e ++ typeName

hiddenPrettify :: Expression d rc -> String
hiddenPrettify e@(Expression n mp) =
    let shape = expressionShape e
        node = expressionNode e
     in case node of
            (Sum _ args) ->
                intercalate "+" . map (hiddenPrettify . flip Expression mp) $
                args
            (Mul _ args) ->
                intercalate ".*" . map (hiddenPrettify . flip Expression mp) $
                args
