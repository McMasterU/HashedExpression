{-# LANGUAGE ScopedTypeVariables #-}

module HashedPrettify where

import qualified Data.ByteString as B
import Data.List (intercalate)
import HashedExpression

prettify :: forall d rc. Expression d rc -> String
prettify e@(Expression n mp) =
    let shape = expressionShape e
        node = expressionNode e
     in case node of
            (Sum _ args) ->
                intercalate "+" . map (prettify . flip Expression mp) $ args
            (Mul _ args) ->
                intercalate ".*" . map (prettify . flip Expression mp) $ args
