module Main where

import AbsHashedLang
import Control.Monad.Except (runExceptT)
import LayoutHashedLang
import LexHashedLang
import ParHashedLang
import PrintHashedLang
import SkelHashedLang
import Symphony
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            content <- readFile fileName
            res <- runExceptT $ parse content
            case res of
                Left error -> print error
                Right ast -> do
                    prob <- runExceptT $ checkSemantic ast
                    print prob
